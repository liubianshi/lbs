# ===========================================================================
# econ-data data lake — read side. See lake-utils.R for shared internals.
# ===========================================================================

#' Read a registered entity from the econ-data data lake.
#'
#' Reads an entity's `data.parquet` via DuckDB and attaches column labels from
#' its `meta.yaml` (using [stlabel()]), matching the attribute shape of
#' [read_archive()]. The entity can be addressed either by an explicit lake
#' `id` or — for back-compatibility with the lbs DBMS archive — by a legacy
#' `(database, table)` pair, which is translated through [lake_id()].
#'
#' @param database lbs database name (e.g. `"CHN_FirmTrade"`). Ignored when
#'   `id` is supplied.
#' @param table lbs table name (e.g. `"HG2005"`). Ignored when `id` is supplied.
#' @param id Optional explicit entity id, `<type>:<domain>/<dataset>@<version>`
#'   (e.g. `"raw:firmtrade/hg@2005"`). Takes precedence over `database`/`table`
#'   and works for any entity type, not just `raw:`.
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
#' @seealso [read_archive()] for the legacy file-based reader; [lake_meta()]
#'   for the entity's full metadata.
#' @export
lake_read <- function(
  database = NULL,
  table = NULL,
  id = NULL,
  var = NULL,
  condition = NULL,
  and = TRUE,
  limit = NULL,
  noinfo = TRUE,
  lake_path = NULL
) {
  lake_root <- .resolve_lake_path(lake_path)

  if (is.null(id)) {
    if (is.null(database) || is.null(table)) {
      stop("lake_read(): supply either `id` or both `database` and `table`.")
    }
    id <- lake_id(database, table)
  }

  entity_dir <- .lake_entity_dir(id, lake_root)
  data_path <- file.path(entity_dir, "data.parquet")
  meta_path <- file.path(entity_dir, "meta.yaml")

  if (!file.exists(data_path)) {
    stop(sprintf(
      "Entity not in lake: %s\n  expected at: %s",
      id, data_path
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

  info <- .lake_columns_dt(.lake_read_meta(meta_path)$columns)
  if (!is.null(var)) info <- info[info$name %in% var]
  if (nrow(info) > 0 && exists("stlabel", mode = "function")) {
    stlabel(data, info[["name"]], info[["label"]])
  }

  if (isTRUE(noinfo)) data else list(data = data, info = info)
}

#' Translate lbs `(database, table)` coordinates to an econ-data raw entity id.
#'
#' Forward-only re-implementation of the rules baked into `gen_mapping.R`
#' (Phase 1 of the DBMS migration). Naming rules:
#' - `source`  = `tolower(database)` with a leading `"chn_"` stripped
#' - `dataset` = `tolower(table)` minus a trailing 4-digit year suffix
#'   (optionally `Q1`–`Q4`)
#' - `version` = the stripped suffix in lowercase, or `"v1"` if absent
#'
#' Examples:
#' - `CHN_FirmTrade`, `HG2005`     -> `raw:firmtrade/hg@2005`
#' - `CHN_FirmTrade`, `HG2005Q4`   -> `raw:firmtrade/hg@2005q4`
#' - `Coding_Inds`,   `HS92_ISIC2` -> `raw:coding_inds/hs92_isic2@v1`
#'
#' @param database lbs database name (e.g. `"CHN_FirmTrade"`).
#' @param table lbs table name (e.g. `"HG2005"`).
#' @return character scalar, the `raw:` entity id.
#' @export
lake_id <- function(database, table) {
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
