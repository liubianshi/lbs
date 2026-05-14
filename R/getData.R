archive_base_dir <- function() {
  base <- Sys.getenv("DATA_ARCHIVE")
  if (nzchar(base)) base else file.path(Sys.getenv("HOME"), "Data", "DBMS")
}

meta_parquet_path <- function(tbl) {
  filename <- switch(
    tbl,
    tables = "_meta_tables.parquet",
    variables = "_meta_variables.parquet",
    stop("unknown meta table: ", tbl)
  )
  file.path(archive_base_dir(), filename)
}

open_meta_con <- function() {
  con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  for (tbl in c("tables", "variables")) {
    p <- meta_parquet_path(tbl)
    if (file.exists(p)) {
      DBI::dbExecute(
        con,
        sprintf("CREATE VIEW meta_%s AS SELECT * FROM '%s'", tbl, p)
      )
    }
  }
  con
}

#' Read a data table from the archive
#'
#' @description *Personal use!* Read a Parquet data file from `$DATA_ARCHIVE`
#'   and attach variable labels from the metadata index.
#' @param database Database name.
#' @param table Table name.
#' @param var Character vector of column names. NULL returns all columns.
#' @param condition Character vector of SQL WHERE conditions.
#' @param path Override the default Parquet file path.
#' @param and Logical. How to combine multiple conditions (TRUE = AND, FALSE = OR).
#' @param limit Integer. SQL LIMIT.
#' @param noinfo If TRUE (default), return only the data. If FALSE, return a
#'   list with `data` and `info`.
#' @return A data.table.
#' @importFrom duckdb duckdb dbConnect dbDisconnect
#' @export
read_archive <- function(
  database,
  table,
  var = NULL,
  condition = NULL,
  path = NULL,
  and = TRUE,
  limit = NULL,
  noinfo = TRUE
) {
  if (is.null(path)) {
    path <- parquet_path(database, table)
  }
  stopifnot(file.exists(path))

  con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(duckdb::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(
    con,
    sprintf("CREATE VIEW %s AS SELECT * FROM '%s'", table, path)
  )

  if (is.null(var)) {
    var <- "*"
    varlist <- "*"
  } else {
    varlist <- paste(var, collapse = ",")
  }

  if (is.null(condition)) {
    condition <- "TRUE"
  } else {
    condition <- paste0("(", condition, ")")
    if (isTRUE(and)) {
      condition <- paste(condition, collapse = " AND \n       ")
    } else if (isFALSE(and)) {
      condition <- paste(condition, collapse = " OR \n       ")
    } else {
      stop("param 'and' only accept TRUE or FALSE")
    }
  }

  if (!is.null(limit)) {
    condition <- paste(condition, "\n LIMIT", limit)
  }

  sel <- gettextf("SELECT %s\n  FROM %s\n WHERE %s", varlist, table, condition)
  message(
    "===============SQL===============\n",
    sel,
    "\n================================="
  )

  data <- DBI::dbGetQuery(con, sel) %>% setDT()
  info <- getdatainfo(database, table, var)
  stlabel(data, names(data), info[, label])

  if (isTRUE(noinfo)) data else list(data = data, info = info)
}

#' @rdname read_archive
#' @param ... arguments forwarded to `read_archive()`.
#' @export
getDataSQLite <- function(...) {
  .Deprecated("read_archive")
  read_archive(...)
}

#' Get variable metadata from the archive index
#'
#' @description *Personal use!* Query variable or table metadata stored in
#'   `_meta_tables.parquet` / `_meta_variables.parquet`.
#' @inheritParams read_archive
#' @return A data.table of metadata rows.
#' @export
getdatainfo <- function(database, table, var = NULL) {
  con <- open_meta_con()
  on.exit(duckdb::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  if (is.null(var)) {
    sel <- gettextf(
      "SELECT * FROM meta_tables WHERE name IN (\n\t%s)",
      paste(paste0("'", database, ":", table, "'"), collapse = ",\n\t")
    )
  } else if (isTRUE(var %in% c("all", "*"))) {
    if (!(length(database) == 1 && length(table) == 1)) {
      stop("Query all variables only allowed in one table")
    }
    sel <- gettextf(
      "SELECT * FROM meta_variables WHERE name LIKE %s",
      paste0("'", database, ":", table, ":%'")
    )
  } else {
    sel <- gettextf(
      "SELECT * FROM meta_variables WHERE name IN (\n\t%s)",
      paste(
        paste0("'", database, ":", table, ":", var, "'"),
        collapse = ",\n\t"
      )
    )
  }

  out <- DBI::dbGetQuery(con, sel) %>% setDT()
  if (length(var) >= 2L) {
    namelist <- strsplit(out$name, ":") %>% purrr::map_chr(`[[`, 3)
    out <- out[match(var, namelist)]
  }
  out
}

#' List all archived variables
#'
#' @description *Personal use!* Return all variable names and labels in the
#'   metadata index.
#' @return A data.table with columns database, table, variable, label.
#' @export
list_variables <- function() {
  con <- open_meta_con()
  on.exit(duckdb::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  vars <- DBI::dbGetQuery(con, "SELECT name, label FROM meta_variables")
  varnames <- vars$name %>% stringr::str_split(":")
  invisible(data.table(
    database = purrr::map_chr(varnames, 1),
    table = purrr::map_chr(varnames, 2),
    variable = purrr::map_chr(varnames, 3),
    label = vars$label
  ))
}

#' @rdname list_variables
#' @export
getallvar <- function() {
  .Deprecated("list_variables")
  list_variables()
}

#' List all archived tables
#'
#' @description *Personal use!* Return all table names and descriptions in the
#'   metadata index.
#' @return A data.table with columns database, table, keys, description.
#' @export
list_tables <- function() {
  con <- open_meta_con()
  on.exit(duckdb::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  tables <- DBI::dbGetQuery(
    con,
    "SELECT name, keys, description FROM meta_tables"
  )
  tablenames <- tables$name %>% stringr::str_split(":")
  databaseList <- purrr::map_chr(tablenames, 1)
  tableList <- purrr::map_chr(tablenames, 2)
  setDT(tables)[, `:=`(
    database = ..databaseList,
    table = ..tableList,
    name = NULL
  )]
  data.table::setcolorder(tables, c("database", "table"))
  tables
}

#' @rdname list_tables
#' @export
getalltable <- function() {
  .Deprecated("list_tables")
  list_tables()
}
