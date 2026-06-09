# ===========================================================================
# econ-data data lake — catalog / metadata side. See lake-utils.R for shared
# internals. Mirrors the verbs of the legacy archive index in getData.R:
#   lake_tables()    cf. list_tables()
#   lake_variables() cf. list_variables()
#   lake_columns()   cf. getdatainfo()
#   lake_meta()      full meta.yaml of one entity
# ===========================================================================

#' List every entity (table) in the data lake.
#'
#' By default scans the entity `meta.yaml` files on disk, so the result is
#' always current. Set `use_registry = TRUE` to instead read the pre-built
#' `registry/entities.parquet` index — faster on a large lake, but only as
#' fresh as the last `econ-data lake index` run. The `dataset` column is
#' derived from the entity id and is the natural join key into
#' [lake_variables()] / [lake_columns()].
#'
#' @param type optional filter on entity type (`raw` / `dim` / `concord` /
#'   `indicator`).
#' @param domain optional filter on domain (e.g. `"firmtrade"`).
#' @param status optional filter on status (`draft` / `stable` / `deprecated`).
#' @param use_registry if `TRUE`, read `registry/entities.parquet` instead of
#'   scanning meta.yaml. Defaults to `FALSE` (always-current disk scan).
#' @param lake_path data lake root. See [lake_read()].
#' @return A `data.table` with columns `id`, `type`, `domain`, `dataset`,
#'   `version`, `name`, `status`, `author`, `updated_at`, `has_data`.
#' @seealso [list_tables()] for the legacy archive index.
#' @export
lake_tables <- function(
  type = NULL,
  domain = NULL,
  status = NULL,
  use_registry = FALSE,
  lake_path = NULL
) {
  reg <- .lake_registry_path(lake_path)

  if (isTRUE(use_registry) && file.exists(reg)) {
    con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
    on.exit(duckdb::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    out <- data.table::setDT(DBI::dbGetQuery(con, sprintf(
      paste(
        "SELECT id, type, domain, name, version, status, author,",
        "       updated_at, has_data",
        "  FROM read_parquet('%s')",
        " ORDER BY type, domain, name, version"
      ),
      reg
    )))
  } else {
    out <- .lake_tables_from_disk(lake_path)
  }

  if (nrow(out) > 0L) {
    out$dataset <- sub("^[^:]+:[^/]+/([^@]+)@.*$", "\\1", out$id)
    data.table::setcolorder(
      out,
      c("id", "type", "domain", "dataset", "version", "name", "status")
    )
  }

  .lake_apply_filters(out, type, domain, status)
}

# Build the table list by scanning meta.yaml files (always current).
.lake_tables_from_disk <- function(lake_path = NULL) {
  lake_root <- .resolve_lake_path(lake_path)
  files <- .lake_meta_files(lake_root)
  rows <- lapply(files, function(f) {
    m <- .lake_read_meta(f)
    if (is.null(m) || is.null(m$id)) return(NULL)
    data.table::data.table(
      id = m$id,
      type = m$type %||% NA_character_,
      domain = m$domain %||% NA_character_,
      name = m$name %||% NA_character_,
      version = as.character(m$version %||% NA_character_),
      status = m$status %||% NA_character_,
      author = m$author %||% NA_character_,
      updated_at = m$updated_at %||% NA_character_,
      has_data = file.exists(file.path(dirname(f), "data.parquet"))
    )
  })
  empty <- data.table::data.table(
    id = character(), type = character(), domain = character(),
    name = character(), version = character(), status = character(),
    author = character(), updated_at = character(), has_data = logical()
  )
  out <- .lake_rbind(rows, empty)
  if (nrow(out) > 0L) {
    data.table::setorderv(out, c("type", "domain", "name", "version"))
  }
  out
}

#' List every column declared across all lake entities.
#'
#' Scans each entity's `meta.yaml` and flattens the `columns` declarations into
#' one long table — the lake analogue of [list_variables()]. Entities without
#' column declarations (most `dim` / `concord` tables) contribute no rows.
#'
#' @param type optional filter on entity type.
#' @param domain optional filter on domain.
#' @param lake_path data lake root. See [lake_read()].
#' @return A `data.table` with columns `id`, `type`, `domain`, `dataset`,
#'   `version`, `column`, `dtype`, `label`.
#' @seealso [lake_columns()] for a single entity; [list_variables()] for the
#'   legacy archive index.
#' @export
lake_variables <- function(type = NULL, domain = NULL, lake_path = NULL) {
  lake_root <- .resolve_lake_path(lake_path)
  files <- .lake_meta_files(lake_root)

  rows <- lapply(files, function(f) {
    m <- .lake_read_meta(f)
    if (is.null(m) || is.null(m$id) || is.null(m$columns)) return(NULL)
    cols <- .lake_columns_dt(m$columns)
    if (nrow(cols) == 0L) return(NULL)
    p <- tryCatch(.parse_entity_id(m$id), error = function(e) NULL)
    if (is.null(p)) return(NULL)
    data.table::data.table(
      id = m$id,
      type = p$type,
      domain = p$domain,
      dataset = p$dataset,
      version = p$version,
      column = cols$name,
      dtype = cols$dtype,
      label = cols$label
    )
  })
  empty <- data.table::data.table(
    id = character(), type = character(), domain = character(),
    dataset = character(), version = character(), column = character(),
    dtype = character(), label = character()
  )
  .lake_apply_filters(.lake_rbind(rows, empty), type, domain)
}

#' List the columns declared in one entity's meta.yaml.
#'
#' Lake analogue of [getdatainfo()]: returns the `name` / `dtype` /
#' `description` (as `label`) for a single entity, without reading its data.
#'
#' @param id entity id, `<type>:<domain>/<dataset>@<version>`.
#' @param var optional character vector to project a subset of columns.
#' @param lake_path data lake root. See [lake_read()].
#' @return A `data.table` with columns `name`, `dtype`, `label`.
#' @seealso [lake_variables()] for all entities; [lake_meta()] for the full
#'   metadata.
#' @export
lake_columns <- function(id, var = NULL, lake_path = NULL) {
  out <- .lake_columns_dt(lake_meta(id, lake_path)$columns)
  if (!is.null(var)) out <- out[out$name %in% var]
  out[]
}

#' Read an entity's full metadata (meta.yaml) from the lake.
#'
#' Returns the parsed `meta.yaml` as a list: identity (`id`, `type`, `domain`,
#' `dataset`, `version`), provenance (`author`, `created_at`, `updated_at`,
#' `source_url`, `license`, `notes`), `status`, and the `columns` declaration.
#'
#' @param id entity id, `<type>:<domain>/<dataset>@<version>`.
#' @param lake_path data lake root. See [lake_read()].
#' @return A named `list` mirroring the YAML structure.
#' @seealso [lake_columns()] for just the column table.
#' @export
lake_meta <- function(id, lake_path = NULL) {
  lake_root <- .resolve_lake_path(lake_path)
  meta_path <- file.path(.lake_entity_dir(id, lake_root), "meta.yaml")
  if (!file.exists(meta_path)) {
    stop(sprintf("meta.yaml not found for %s\n  expected at: %s", id, meta_path))
  }
  meta <- .lake_read_meta(meta_path)
  if (is.null(meta)) stop("Failed to parse meta.yaml at ", meta_path)
  meta
}
