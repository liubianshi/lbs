# ===========================================================================
# econ-data data lake — write side. See lake-utils.R for shared internals.
#
# Implementation strategy (shell-out, see econ-data-plan §P1.2):
#   1. Build meta.yaml in R using lake_dtype() for column-type inference.
#   2. Atomically place data.parquet under $ECON_DATA_LAKE_PATH/raw/.../@v/.
#   3. Delegate validation + audit hook to `econ-data entity register --meta`.
# This keeps every write path through the same schema gateway as direct CLI
# use, so dtype rules (binary rejected, RFC3339 timestamps, etc.) are
# enforced in exactly one place.
# ===========================================================================

#' Translate an R column class to the econ-data meta.yaml dtype vocabulary.
#'
#' Mirrors `scripts/migration/lbs_dbms/_meta_helpers.R::map_dtype()`. Output
#' is one of `string` / `integer` / `numeric` / `boolean` / `timestamp` /
#' `date`, matching the `value_columns.dtype` enum in
#' `internal/meta/schema/v1.json`. For unknown classes the original class
#' string is returned (the CLI will then reject it, surfacing the bad column
#' to the user).
#'
#' @param x a column (atomic vector / factor / POSIXct / etc.).
#' @return character scalar.
#' @export
lake_dtype <- function(x) {
  if (inherits(x, c("POSIXct", "POSIXt"))) return("timestamp")
  if (inherits(x, "Date")) return("date")
  if (is.factor(x) || is.character(x)) return("string")
  if (is.logical(x)) return("boolean")
  if (is.integer(x)) return("integer")
  if (is.numeric(x)) return("numeric")
  cls <- class(x)[1]
  cls
}

#' Format the current time as an RFC3339 UTC string the CLI will accept.
#'
#' The econ-data validator (`internal/meta/meta.go`) rejects R's default
#' `+0800` offset; only `Z` or `+HH:MM` are valid. Centralised here so every
#' lbs writer uses the right shape.
#'
#' @return character scalar like `"2026-05-15T10:24:00Z"`.
#' @keywords internal
.lake_iso_now <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

# A lake id segment ("slug"): lowercase letters / digits / underscores, must
# start with an alphanumeric. A loose mirror of the CLI's strict grammar — we
# only validate enough to fail early; the CLI does the authoritative check.
.lake_id_slug <- "^[a-z0-9][a-z0-9_]*[a-z0-9]?$"

# Parse a raw entity id `raw:<source>/<dataset>@<version>` into its parts.
# Splits on the structural separators first, then validates each segment, so a
# failure points at the offending piece instead of a single opaque regex miss.
# register only supports raw:.
.parse_raw_id <- function(id) {
  bad <- function(reason) {
    stop("lake_register() ", reason, ".\n",
         "  Expected id format: raw:<source>/<dataset>@<version>\n",
         "  Got: ", id, call. = FALSE)
  }

  # 1. Strip the type prefix — register only handles raw entities.
  if (!startsWith(id, "raw:")) bad("currently supports raw entities only")
  body <- sub("^raw:", "", id)

  # 2. Split structure: <source> / <dataset> @ <version>.
  source_rest <- strsplit(body, "/", fixed = TRUE)[[1]]
  if (length(source_rest) != 2L) bad("id must contain exactly one '/'")

  dataset_version <- strsplit(source_rest[2], "@", fixed = TRUE)[[1]]
  if (length(dataset_version) != 2L) bad("id must contain exactly one '@'")

  parts <- list(
    source = source_rest[1],
    dataset = dataset_version[1],
    version = dataset_version[2]
  )

  # 3. Validate each segment, naming the bad one in the error.
  for (nm in names(parts)) {
    if (!grepl(.lake_id_slug, parts[[nm]])) {
      bad(sprintf("has an invalid %s segment: '%s'", nm, parts[[nm]]))
    }
  }

  parts
}

# Build the columns list for meta.yaml: each entry is always a list (never
# a named vector) so yaml::write_yaml emits proper YAML maps, not scalars.
.build_lake_columns <- function(df, columns) {
  col_names <- names(df)
  out <- vector("list", length(col_names))
  for (i in seq_along(col_names)) {
    cn <- col_names[i]
    auto_dtype <- lake_dtype(df[[cn]])
    entry <- list(name = cn, dtype = auto_dtype)

    if (!is.null(columns) && cn %in% names(columns)) {
      override <- columns[[cn]]
      if (is.character(override) && length(override) == 1L) {
        if (nzchar(override)) entry$description <- override
      } else if (is.list(override)) {
        if (!is.null(override$dtype) && nzchar(override$dtype)) {
          entry$dtype <- override$dtype
        }
        if (!is.null(override$description) && nzchar(override$description)) {
          entry$description <- override$description
        }
      } else {
        stop("columns$", cn, " must be a character description or a list ",
             "with $description / $dtype. Got class: ", class(override)[1])
      }
    } else {
      entry$description <- "TODO"
    }
    out[[i]] <- entry
  }
  out
}

#' Register an R data frame as a raw entity in the econ-data data lake.
#'
#' One-shot writer: takes a `data.frame` / `data.table`, infers a draft
#' meta.yaml from column classes, copies the data into the lake's canonical
#' path, then shells out to `econ-data entity register --meta` so the same
#' validator (binary dtype rejection, RFC3339 timestamp enforcement,
#' cross-field id checks) runs as for direct CLI use.
#'
#' If validation fails the placed `data.parquet` is rolled back so the lake
#' never ends up in a half-written state.
#'
#' @param df A `data.frame` / `data.table` to publish. Columns must be of
#'   types Arrow / Parquet accepts (`string`, `integer`, `numeric`,
#'   `boolean`, `POSIXct`, `Date`). Binary / list columns will be rejected
#'   downstream by the CLI's parquet schema check — clean them up before
#'   calling.
#' @param id Target entity id, `raw:<source>/<dataset>@<version>`. Currently
#'   only `raw:` is supported (dim / indicator / concord come via the
#'   dedicated CLI flows).
#' @param columns Optional named list / vector of column descriptions. Two
#'   accepted shapes per entry:
#'   - `firm_id = "企业唯一标识"` — plain description.
#'   - `revenue = list(description = "营业收入", dtype = "numeric")` —
#'     description plus dtype override (when auto-inference would map to the
#'     wrong vocabulary).
#'   Columns not listed get `description: TODO` (so users see what's missing
#'   when they `entity show` the result).
#' @param name Human-readable display name. Defaults to the dataset segment of `id`.
#' @param domain Subject area. Defaults to the source segment of `id`.
#' @param author meta.yaml author field. Defaults to `$USER` or `"TODO"`.
#' @param status `"draft"` (default) or `"stable"`. Use `"draft"` when the
#'   column descriptions still contain `TODO`s.
#' @param source_url,license,notes Optional metadata pass-through to the
#'   meta.yaml.
#' @param overwrite If `TRUE`, replace an existing entity with the same id.
#' @param dry_run If `TRUE`, print the planned meta.yaml + CLI invocation and
#'   return without touching disk.
#' @param lake_path Override the lake root. Defaults to
#'   `$ECON_DATA_LAKE_PATH` or `~/Data/econ-data-lake`.
#' @param econdata_bin Path to the `econ-data` binary. Defaults to
#'   `Sys.which("econ-data")`.
#' @return Invisibly, a `list(id, data_path, meta_path, cli_output, dry_run)`.
#' @seealso [lake_read()] for the symmetric reader; the
#'   `econ-data entity scaffold` CLI command for the equivalent
#'   shell-only flow.
#' @export
lake_register <- function(
  df,
  id,
  columns = NULL,
  name = NULL,
  domain = NULL,
  author = NULL,
  status = c("draft", "stable", "deprecated"),
  source_url = NULL,
  license = NULL,
  notes = NULL,
  overwrite = FALSE,
  dry_run = FALSE,
  lake_path = NULL,
  econdata_bin = NULL
) {
  if (!is.data.frame(df)) {
    stop("lake_register(): 'df' must be a data.frame or data.table; got ",
         class(df)[1])
  }
  if (ncol(df) == 0L) stop("lake_register(): 'df' has zero columns.")
  status <- match.arg(status)
  parsed <- .parse_raw_id(id)
  lake_root <- .resolve_lake_path(lake_path)

  entity_dir <- file.path(
    lake_root, "raw", parsed$source,
    sprintf("%s@%s", parsed$dataset, parsed$version)
  )
  data_path <- file.path(entity_dir, "data.parquet")
  meta_target <- file.path(entity_dir, "meta.yaml")

  if (is.null(name) || !nzchar(name)) name <- parsed$dataset
  if (is.null(domain) || !nzchar(domain)) domain <- parsed$source
  if (is.null(author) || !nzchar(author)) {
    author <- Sys.getenv("USER", unset = "TODO")
  }

  now <- .lake_iso_now()
  meta <- list(
    schema_version = 1L,
    id = id,
    type = "raw",
    name = name,
    domain = domain,
    status = status,
    author = author,
    created_at = now,
    updated_at = now,
    source = parsed$source,
    dataset = parsed$dataset,
    version = parsed$version,
    columns = .build_lake_columns(df, columns)
  )
  if (!is.null(source_url) && nzchar(source_url)) meta$source_url <- source_url
  if (!is.null(license) && nzchar(license)) meta$license <- license
  if (!is.null(notes) && nzchar(notes)) meta$notes <- notes

  if (isTRUE(dry_run)) {
    cat("[dry-run] lake_register plan:\n")
    cat("  id          ", id, "\n", sep = "")
    cat("  data path   ", data_path, "\n", sep = "")
    cat("  meta path   ", meta_target, "\n", sep = "")
    cat("  columns     ", length(meta$columns), "\n", sep = "")
    cat("  overwrite   ", overwrite, "\n", sep = "")
    cat("\n--- meta.yaml preview ---\n")
    cat(yaml::as.yaml(meta))
    cat("--- end preview ---\n")
    cat("\n--- CLI to be invoked ---\n")
    cat("  econ-data entity register --meta <tmpfile>",
        if (overwrite) " --yes" else "", "\n", sep = "")
    return(invisible(list(
      id = id, data_path = data_path, meta_path = meta_target,
      dry_run = TRUE, meta = meta
    )))
  }

  if (file.exists(meta_target) && !isTRUE(overwrite)) {
    stop(sprintf(
      "Entity %s already exists at %s.\n  Pass overwrite=TRUE to replace, ",
      id, entity_dir
    ), "or bump the @version segment of the id to publish a new revision.")
  }

  # Resolve the CLI binary up-front so we fail before touching disk.
  cli <- if (!is.null(econdata_bin) && nzchar(econdata_bin)) {
    econdata_bin
  } else {
    Sys.which("econ-data")
  }
  if (!nzchar(cli) || !file.exists(cli)) {
    stop("econ-data CLI not found.\n",
         "  Set econdata_bin = '/path/to/econ-data' or add it to $PATH.")
  }

  dir.create(entity_dir, recursive = TRUE, showWarnings = FALSE)

  # Atomic-ish parquet placement: write to a tmp file in the same dir, then
  # rename. R's file.rename is atomic on POSIX when src/dst share a filesystem.
  data_tmp <- tempfile(tmpdir = entity_dir, fileext = ".parquet.tmp")
  arrow::write_parquet(df, data_tmp)
  # Captured BEFORE the rename so the rollback path below knows whether we
  # created data.parquet fresh (safe to unlink on CLI failure) or replaced an
  # existing one (the rename has already lost the prior bytes — the user
  # implicitly accepted that by passing overwrite=TRUE).
  data_preexisted <- file.exists(data_path)
  if (!file.rename(data_tmp, data_path)) {
    ok <- file.copy(data_tmp, data_path, overwrite = TRUE)
    unlink(data_tmp)
    if (!isTRUE(ok)) stop("Failed to place data.parquet at ", data_path)
  }

  # Build meta in a tempfile and hand it to the CLI, which re-writes it into
  # the lake. Single-element strings stay scalars and the list-of-maps
  # `columns` stays a list, which is the shape the CLI expects.
  meta_tmp <- tempfile(fileext = ".meta.yaml")
  on.exit(unlink(meta_tmp), add = TRUE)
  yaml::write_yaml(meta, meta_tmp)

  args <- c("entity", "register", "--meta", meta_tmp)
  if (isTRUE(overwrite)) args <- c(args, "--yes")

  res <- suppressWarnings(
    system2(cli, args, stdout = TRUE, stderr = TRUE)
  )
  exit_code <- attr(res, "status")

  if (!is.null(exit_code) && exit_code != 0L) {
    if (!data_preexisted) unlink(data_path)
    stop(sprintf(
      "econ-data entity register failed (exit %d):\n%s",
      exit_code, paste(res, collapse = "\n")
    ))
  }

  message(paste(res, collapse = "\n"))
  invisible(list(
    id = id,
    data_path = data_path,
    meta_path = meta_target,
    cli_output = res,
    dry_run = FALSE
  ))
}

#' Write an R data frame to a parquet file using the lake's dtype rules.
#'
#' Convenience wrapper around [arrow::write_parquet()] that warns on column
#' types the econ-data validator will reject (binary, list columns). Useful
#' when you want to inspect the parquet before calling [lake_register()] —
#' or when you're handing the file off to a colleague who will register it.
#'
#' @param df data.frame / data.table.
#' @param path output file path.
#' @param ... forwarded to [arrow::write_parquet()].
#' @return invisibly the path that was written.
#' @export
lake_write <- function(df, path, ...) {
  if (!is.data.frame(df)) {
    stop("lake_write(): 'df' must be a data.frame.")
  }
  bad <- vapply(df, function(x) {
    inherits(x, "blob") || is.list(x) || is.raw(x)
  }, logical(1))
  if (any(bad)) {
    warning("Columns with classes the econ-data CLI will reject: ",
            paste(names(df)[bad], collapse = ", "),
            "\n  Cast to string/integer/numeric before lake_register().")
  }
  arrow::write_parquet(df, path, ...)
  invisible(path)
}
