#'  Get data from SQLite Database
#'
#' @description 从 SQLite 数据库中获取数据表
#' @param database SQLite database path
#' @param table Table name
#' @param var A character vector consist of column names.
#' @param condition A character vector consist of SQL conditions
#' @param and Logical value. Way of combine conditions
#' @param limit A interger. SQL Limit.
#' @return A data.frame
#' @export
getDataSQLite <- function(database, table, var = NULL,
                       condition = NULL, path = NULL,
                       and = TRUE, limit = NULL, noinfo = TRUE) {
    if (is.null(path)) {
        path <- file.path(Sys.getenv("DATA_ARCHIVE"),
                          paste0(database, ".sqlite"))
    }
    con <- DBI::dbConnect(RSQLite::SQLite(), path)
    on.exit(DBI::dbDisconnect(con))

    tableList <- DBI::dbListTables(con)
    stopifnot(isTRUE(table %in% tableList))

    if (is.null(var))  {
        var <- "*"
        varlist <-  "*"
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
            condition <- paste(condtion, collapse = " OR \n       ")
        } else {
            stop("param 'and' only accept TRUE or FALSE")
        }
    }

    if (!is.null(limit))
        condition <- paste(condition, "\n LIMIT", limit)

    sel <- gettextf("SELECT %s\n  FROM %s\n WHERE %s", varlist, table, condition)
    message("===============SQL===============\n",
            sel, '\n=================================')

    data <- DBI::dbGetQuery(con, sel) %>% setDT()
    info <- getdatainfo(database, table, var)
    stlabel(data, names(data), info[, label])

    if (isTRUE(noinfo)) {
        data
    } else {
        list(data = data, info = info)
    }
}

#' Get infomation form data.repo
#'
#' @description get data information from srdm repo
#' @inheritParams getDataSQLite
#' @return return a data.frame contain data info
#' @export
getdatainfo <- function(database, table, var = NULL) {
    database_path <- Sys.getenv("SRDM_DATA_REPO_PATH")
    con <- DBI::dbConnect(RSQLite::SQLite(), database_path)
    on.exit(DBI::dbDisconnect(con))

    if (is.null(var)) {
        sel <- gettextf("SELECT * FROM data_table WHERE name IN (\n\t%s)",
                        paste(paste0("'", database, ":", table, "'"), collapse = ",\n\t"))
    } else if (isTRUE(var %in% c("all", "*"))) {
        if (!(length(database) == 1 && length(table) == 1))
            stop("Query all variables only allowed in one table")
        sel <- gettextf("SELECT * FROM data_record WHERE name LIKE %s",
            paste0("'", database, ":", table, ":%'")
        )
    } else {
        sel <- gettextf("SELECT * FROM data_record WHERE name IN (\n\t%s)",
                        paste(paste0("'", database, ":", table, ":", var, "'"), collapse = ",\n\t"))
    }
    #message("SQL Query Sentence:\n", sel)
    out <- DBI::dbGetQuery(con, sel) %>% setDT()
    if (length(var) >= 2L) {
        namelist <- strsplit(out$name, ":") %>% purrr::map_chr(`[[`, 3)
        out <- out[match(var, namelist)]
    }
    out
}

#' get all variables' basic infomation in srmd repo
#'
#' @return All variable name and label stored in srdm repo
#' @export
getallvar <- function() {
    database_path <- Sys.getenv("SRDM_DATA_REPO_PATH")
    con <- DBI::dbConnect(RSQLite::SQLite(), database_path)
    on.exit(DBI::dbDisconnect(con))

    vars         <- DBI::dbGetQuery(con, "SELECT name,label from data_record")
    varnames     <- vars$name %>% stringr::str_split(":")
    varLabels    <- vars$label
    databaseList <- purrr::map_chr(varnames, 1)
    tableList    <- purrr::map_chr(varnames, 2)
    varList      <- purrr::map_chr(varnames, 3)
    invisible(data.table(database = databaseList,
                         table = tableList,
                         variable = varList,
                         label = varLabels
                         ))
}

#' get all tables' basic information from srmd_repo
#'
#' @return All tables name and label stored in srdm repo
#' @export
getalltable <- function() {
    database_path <- Sys.getenv("SRDM_DATA_REPO_PATH")
    con <- DBI::dbConnect(RSQLite::SQLite(), database_path)
    on.exit(DBI::dbDisconnect(con))

    tables       <- DBI::dbGetQuery(con, "SELECT name,keys,description from data_table")
    tablenames   <- tables$name %>% stringr::str_split(":")
    databaseList <- purrr::map_chr(tablenames, 1)
    tableList    <- purrr::map_chr(tablenames, 2)
    setDT(tables)[, `:=`(database = ..databaseList,
                         table    = ..tableList,
                         name     = NULL)]
    data.table::setcolorder(tables, c("database", "table"))
    tables
}
