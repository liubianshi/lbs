#' Read a registered raw entity from the econ-data data lake.
#'
#' Compatibility shim during the migration from lbs's `~/Data/DBMS` archive
#' to the econ-data data lake. Translates lbs `(database, table)` coordinates
#' to the lake's `raw:<source>/<dataset>@<version>` id format and reads the
#' corresponding `data.parquet`. Column labels stored in the entity's
#' `meta.yaml` are attached to the result via [stlabel()], matching the
#' attribute shape of [read_archive()].
#'
#' Naming rules mirror the migration generator (`gen_mapping.R`):
#' - `source`  = `tolower(database)` with a leading `"chn_"` stripped
#' - `dataset` = `tolower(table)` minus a trailing 4-digit year suffix
#'   (optionally `Q1`–`Q4`)
#' - `version` = the stripped suffix in lowercase, or `"v1"` if absent
#'
#' Examples of the mapping:
#' - `CHN_FirmTrade`, `HG2005`      → `raw:firmtrade/hg@2005`
#' - `CHN_FirmTrade`, `HG2005Q4`    → `raw:firmtrade/hg@2005q4`
#' - `Coding_Inds`,   `HS92_ISIC2`  → `raw:coding_inds/hs92_isic2@v1`
#'
#' @param database lbs database name (e.g. `"CHN_FirmTrade"`).
#' @param table lbs table name (e.g. `"HG2005"`).
#' @param var optional character vector of column names to project.
#' @param condition optional SQL `WHERE` clause(s); same semantics as
#'   [read_archive()].
#' @param and `TRUE` (default) joins `condition` with `AND`; `FALSE` with `OR`.
#' @param limit optional integer row cap.
#' @param noinfo if `TRUE` (default) return the `data.table` directly; if
#'   `FALSE` return `list(data, info)` matching [read_archive()].
#' @param lake_path data lake root. Defaults to the env var
#'   `ECON_DATA_LAKE_PATH`, falling back to `~/Data/econ-data-lake`.
#' @return `data.table` (or `list(data, info)` if `noinfo = FALSE`).
#' @seealso [read_archive()] for the legacy file-based reader.
#' @export
read_archive_lake <- function(
  database,
  table,
  var = NULL,
  condition = NULL,
  and = TRUE,
  limit = NULL,
  noinfo = TRUE,
  lake_path = NULL
) {
  if (is.null(lake_path)) {
    lake_path <- Sys.getenv(
      "ECON_DATA_LAKE_PATH",
      unset = file.path(Sys.getenv("HOME"), "Data", "econ-data-lake")
    )
  }

  id <- lbs_to_raw_id(database, table)
  entity_dir <- file.path(lake_path, sub("^raw:", "raw/", id))
  data_path <- file.path(entity_dir, "data.parquet")
  meta_path <- file.path(entity_dir, "meta.yaml")

  if (!file.exists(data_path)) {
    stop(sprintf(
      "Entity not in lake: %s\n  expected at: %s\n  (database=%s, table=%s)",
      id,
      data_path,
      database,
      table
    ))
  }

  varlist <- if (is.null(var)) "*" else paste(var, collapse = ", ")

  if (is.null(condition)) {
    where <- "TRUE"
  } else {
    condition <- paste0("(", condition, ")")
    where <- if (isTRUE(and)) {
      paste(condition, collapse = " AND \n       ")
    } else if (isFALSE(and)) {
      paste(condition, collapse = " OR \n       ")
    } else {
      stop("param 'and' only accept TRUE or FALSE")
    }
  }
  if (!is.null(limit)) where <- paste(where, "\n LIMIT", limit)

  sel <- sprintf(
    "SELECT %s FROM read_parquet('%s') WHERE %s",
    varlist,
    data_path,
    where
  )

  con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(duckdb::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  data <- data.table::setDT(DBI::dbGetQuery(con, sel))

  info <- lake_columns_info(meta_path, var)
  if (nrow(info) > 0 && exists("stlabel", mode = "function")) {
    stlabel(data, info[["name"]], info[["label"]])
  }

  if (isTRUE(noinfo)) data else list(data = data, info = info)
}

#' Translate lbs `(database, table)` coordinates to an econ-data raw entity id.
#'
#' Forward-only re-implementation of the rules baked into `gen_mapping.R`
#' (Phase 1 of the DBMS migration). Exported so callers can preview the
#' resolved id without performing the read.
#'
#' @inheritParams read_archive_lake
#' @return character scalar, the `raw:` entity id.
#' @export
lbs_to_raw_id <- function(database, table) {
  src <- tolower(database)
  src <- sub("^chn_", "", src)
  src <- gsub("[^a-z0-9_]", "_", src)
  src <- gsub("_+", "_", src)
  src <- gsub("^_+|_+$", "", src)

  tbl <- tolower(table)
  m <- regmatches(tbl, regexec("^(.*?)([0-9]{4}(?:[qQ][1-4])?)$", tbl))[[1]]
  if (length(m) == 3 && nzchar(m[2])) {
    ds <- m[2]
    ver <- m[3]
  } else {
    ds <- tbl
    ver <- "v1"
  }
  ds <- gsub("[^a-z0-9_]", "_", ds)
  ds <- gsub("_+", "_", ds)
  ds <- gsub("^_+|_+$", "", ds)

  sprintf("raw:%s/%s@%s", src, ds, ver)
}

# Parse meta.yaml columns array into a small data.table { name, label }.
# Internal helper, not exported.
lake_columns_info <- function(meta_path, var = NULL) {
  empty <- data.table::data.table(name = character(), label = character())
  if (!file.exists(meta_path)) return(empty)

  m <- tryCatch(
    yaml::read_yaml(meta_path),
    error = function(e) NULL
  )
  if (is.null(m) || is.null(m$columns) || length(m$columns) == 0) return(empty)

  names_v <- vapply(
    m$columns,
    function(c) if (is.null(c$name)) NA_character_ else c$name,
    character(1)
  )
  labels_v <- vapply(
    m$columns,
    function(c) {
      if (is.null(c$description) || identical(c$description, "")) {
        NA_character_
      } else {
        c$description
      }
    },
    character(1)
  )

  out <- data.table::data.table(name = names_v, label = labels_v)
  if (!is.null(var)) out <- out[out$name %in% var]
  out
}


# ---------------------------------------------------------------------------
# Write side: register a data.frame as a raw entity in the econ-data lake.
#
# Implementation strategy (shell-out, see econ-data-plan §P1.2):
#   1. Build meta.yaml in R using map_lake_dtype() for column-type inference.
#   2. Atomically place data.parquet under $ECON_DATA_LAKE_PATH/raw/.../@v/.
#   3. Delegate validation + audit hook to `econ-data entity register --meta`.
# This keeps every write path through the same schema gateway as direct CLI
# use, so dtype rules (binary rejected, RFC3339 timestamps, etc.) are
# enforced in exactly one place.
# ---------------------------------------------------------------------------

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
map_lake_dtype <- function(x) {
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

# Parse a raw entity id into its components. Concise validator — does NOT
# replicate the full pkg/entityid grammar; just enough to fail early before
# we touch disk. The CLI does the authoritative check.
.parse_raw_id <- function(id) {
  m <- regmatches(id, regexec("^raw:([a-z0-9][a-z0-9_]*[a-z0-9]?)/([a-z0-9][a-z0-9_]*[a-z0-9]?)@([a-z0-9][a-z0-9_]*[a-z0-9]?)$", id))[[1]]
  if (length(m) != 4 || !nzchar(m[2])) {
    stop("register_lake() currently supports raw entities only.\n",
         "  Expected id format: raw:<source>/<dataset>@<version>\n",
         "  Got: ", id)
  }
  list(source = m[2], dataset = m[3], version = m[4])
}

# Resolve the lake root: explicit arg > ECON_DATA_LAKE_PATH > ~/Data/econ-data-lake.
.resolve_lake_path <- function(lake_path) {
  if (!is.null(lake_path) && nzchar(lake_path)) return(lake_path)
  Sys.getenv("ECON_DATA_LAKE_PATH",
             unset = file.path(Sys.getenv("HOME"), "Data", "econ-data-lake"))
}

# Build the columns list for meta.yaml: each entry is always a list (never
# a named vector) so yaml::write_yaml emits proper YAML maps, not scalars.
.build_lake_columns <- function(df, columns) {
  col_names <- names(df)
  out <- vector("list", length(col_names))
  for (i in seq_along(col_names)) {
    cn <- col_names[i]
    auto_dtype <- map_lake_dtype(df[[cn]])
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

# Write meta as YAML using yaml::write_yaml, ensuring single-element string
# fields stay scalars and list-of-maps stay lists.
.write_meta_yaml <- function(meta, path) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required for register_lake(); install.packages('yaml').")
  }
  yaml::write_yaml(meta, path)
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
#' @seealso [read_archive_lake()] for the symmetric reader; the
#'   `econ-data entity scaffold` CLI command for the equivalent
#'   shell-only flow.
#' @export
register_lake <- function(
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
    stop("register_lake(): 'df' must be a data.frame or data.table; got ",
         class(df)[1])
  }
  if (ncol(df) == 0L) stop("register_lake(): 'df' has zero columns.")
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required for register_lake(); install.packages('arrow').")
  }
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
    cat("[dry-run] register_lake plan:\n")
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

  # Build meta in a tempfile; pass to CLI. The CLI re-writes it into the lake.
  meta_tmp <- tempfile(fileext = ".meta.yaml")
  on.exit(unlink(meta_tmp), add = TRUE)
  .write_meta_yaml(meta, meta_tmp)

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
#' when you want to inspect the parquet before calling [register_lake()] —
#' or when you're handing the file off to a colleague who will register it.
#'
#' @param df data.frame / data.table.
#' @param path output file path.
#' @param ... forwarded to [arrow::write_parquet()].
#' @return invisibly the path that was written.
#' @export
write_lake <- function(df, path, ...) {
  if (!is.data.frame(df)) {
    stop("write_lake(): 'df' must be a data.frame.")
  }
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required for write_lake(); install.packages('arrow').")
  }
  bad <- vapply(df, function(x) {
    inherits(x, "blob") || is.list(x) || is.raw(x)
  }, logical(1))
  if (any(bad)) {
    warning("Columns with classes the econ-data CLI will reject: ",
            paste(names(df)[bad], collapse = ", "),
            "\n  Cast to string/integer/numeric before register_lake().")
  }
  arrow::write_parquet(df, path, ...)
  invisible(path)
}
