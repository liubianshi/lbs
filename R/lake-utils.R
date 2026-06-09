# ===========================================================================
# R-side interface for the econ-data data lake — shared internals.
#
# The lake lives at $ECON_DATA_LAKE_PATH (default ~/Data/econ-data-lake) and
# is laid out as <type>/<domain>/<dataset>@<version>/{data.parquet, meta.yaml},
# with a pre-built index at registry/entities.parquet. We read those files
# directly with DuckDB / yaml rather than shelling out to `econ-data`, except
# on the register path where the CLI is the authoritative schema gateway.
#
# The exported `lake_*` surface is split across sibling files:
#   - lake-read.R     lake_read()  / lake_id()
#   - lake-catalog.R  lake_tables() / lake_variables() / lake_columns() /
#                     lake_meta()
#   - lake-write.R    lake_register() / lake_write() / lake_dtype()
# This file holds the path / id / meta helpers they all share. None are
# exported.
# ===========================================================================

# Resolve the lake root: explicit arg > ECON_DATA_LAKE_PATH > ~/Data/econ-data-lake.
.resolve_lake_path <- function(lake_path = NULL) {
  if (!is.null(lake_path) && nzchar(lake_path)) return(lake_path)
  Sys.getenv(
    "ECON_DATA_LAKE_PATH",
    unset = file.path(Sys.getenv("HOME"), "Data", "econ-data-lake")
  )
}

# Parse any entity id into (type, domain, dataset, version). Concise validator
# covering all four entity types — the CLI does the authoritative check.
.parse_entity_id <- function(id) {
  m <- regmatches(
    id,
    regexec("^(raw|dim|concord|indicator):([^/]+)/([^@]+)@(.+)$", id)
  )[[1]]
  if (length(m) != 5L) {
    stop(
      "Not a valid econ-data entity id.\n",
      "  Expected: <type>:<domain>/<dataset>@<version>\n",
      "  Got: ", id
    )
  }
  list(type = m[2], domain = m[3], dataset = m[4], version = m[5])
}

# Map an entity id to its on-disk directory under the lake root.
.lake_entity_dir <- function(id, lake_root) {
  p <- .parse_entity_id(id)
  file.path(
    lake_root, p$type, p$domain,
    sprintf("%s@%s", p$dataset, p$version)
  )
}

# Path to the lake registry index (entities.parquet). Not exported.
.lake_registry_path <- function(lake_path = NULL) {
  file.path(.resolve_lake_path(lake_path), "registry", "entities.parquet")
}

# Glob every entity meta.yaml under the four typed sub-trees. Returns absolute
# paths; the registry directory has none, so it is naturally excluded.
.lake_meta_files <- function(lake_root) {
  dirs <- file.path(lake_root, c("raw", "dim", "concord", "indicator"))
  dirs <- dirs[dir.exists(dirs)]
  if (!length(dirs)) return(character())
  list.files(
    dirs,
    pattern = "^meta\\.yaml$",
    recursive = TRUE,
    full.names = TRUE
  )
}

# Turn a meta.yaml `columns` list into a data.table {name, dtype, label}.
# `label` is the column `description`; empty descriptions become NA.
.lake_columns_dt <- function(columns) {
  empty <- data.table::data.table(
    name = character(), dtype = character(), label = character()
  )
  if (is.null(columns) || length(columns) == 0L) return(empty)

  pull <- function(field) {
    vapply(
      columns,
      function(c) {
        v <- c[[field]]
        if (is.null(v) || identical(v, "")) NA_character_ else as.character(v)
      },
      character(1)
    )
  }
  data.table::data.table(
    name = pull("name"),
    dtype = pull("dtype"),
    label = pull("description")
  )
}

# Read + parse one meta.yaml. Returns NULL on missing/unparseable file.
.lake_read_meta <- function(meta_path) {
  if (!file.exists(meta_path)) return(NULL)
  tryCatch(yaml::read_yaml(meta_path), error = function(e) NULL)
}

# Bind a list of per-entity data.tables into one. rbindlist already drops NULL
# entries, so we only special-case the all-empty result, returning `empty` (a
# 0-row template) to preserve the declared column schema.
.lake_rbind <- function(rows, empty) {
  out <- data.table::rbindlist(rows, use.names = TRUE)
  if (nrow(out) == 0L) empty else out
}

# Apply optional equality filters to a catalog table. Masks are built in this
# frame (not inside `dt[...]`) so data.table's NSE cannot shadow the filter
# arguments with the same-named columns.
.lake_apply_filters <- function(dt, type = NULL, domain = NULL, status = NULL) {
  keep <- rep(TRUE, nrow(dt))
  if (!is.null(type)) keep <- keep & dt$type %in% type
  if (!is.null(domain)) keep <- keep & dt$domain %in% domain
  if (!is.null(status)) keep <- keep & dt$status %in% status
  dt[keep]
}
