#'  Get data from SQLite Database
#'
#' @description 从 SQLite 数据库中获取数据表
#' @param database SQLite database path
#' @param table Table name
#' @param varlist A character vector consist of column names.
#' @param condition A character vector consist of SQL conditions
#' @param and Logical value. Way of combine conditions
#' @param limit A interger. SQL Limit.
#' @return A data.frame
#' @export
getDataSQLite <- function(database, table, varlist = NULL,
                       condition = NULL, path = NULL, and = TRUE, limit = NULL) {
    if (is.null(path)) {
        path <- if (Sys.getenv("DATA_ARCHIVE") != "") {
            file.path(Sys.getenv("DATA_ARCHIVE"), paste0(database, ".sqlite"))
        } else {
            file.path(Sys.getenv("HOME"), "Data", "DBMS",
                      paste0(database, ".sqlite"))
        }
    }
    con <- DBI::dbConnect(RSQLite::SQLite(), path)
    on.exit(DBI::dbDisconnect(con))

    tableList <- DBI::dbListTables(con)
    if (!isTRUE(table %in% tableList)) stop("table isnot exist")

    if (is.null(varlist))  {
        varlist   <-  "*"
    } else {
        varlist <- paste(varlist, collapse = ",")
    }

    if (is.null(condition)) {
        condition <- "TRUE"
    } else {
        condition <- paste0("(", condition, ")")
        if (isTRUE(and)) {
            condition <- paste(condition, collapse = " AND ")
        } else if (isFALSE(and)) {
            condition <- paste(condtion, collapse = " OR ")
        } else {
            stop("param 'and' only accept TRUE or FALSE") 
        }
    }

    if (!is.null(limit))
        condition <- paste(condition, "LIMIT", limit)

    sel <- gettextf("SELECT %s FROM %s WHERE %s", varlist, table, condition)
    message(sel)

    data <- DBI::dbGetQuery(con, sel)
    info <- rbindlist(lapply(names(data), getdatainfo,
                   database = database, table = table))
    invisible(list(data = data, info = info))
}

#' Get infomation form data.repo
#'
#' @description 从数据管理库获取数据信息
#' @inheritParams getDataSQLite
#' @return 如果查询变量的信息，那么返回一个单行数据框，
#'         如果查询表格信息，那么返回表格本身信息和所有变量信息构成的列表
#' @export
getdatainfo <- function(database, table, var = NULL) {
    database_path <- file.path(Sys.getenv("SRDM_DATA_REPO_PATH"), "srdm_dataRepo.sqlite")
    con <- DBI::dbConnect(RSQLite::SQLite(), database_path)
    on.exit(DBI::dbDisconnect(con))

    if (is.null(var)) {
        sel_table <- gettextf("SELECT * FROM data_table WHERE name == '%s'",
                              paste(database, table, sep = ":"))
        sel_varlist <- gettextf("SELECT * FROM data_record WHERE name LIKE '%s:%%'",
                                paste(database, table, sep = ":"))
        out <- list(
            table = DBI::dbGetQuery(con, sel_table),
            varlist = DBI::dbGetQuery(con, sel_varlist)
        )
    } else {
        sel <- gettextf("SELECT * FROM data_record WHERE name == '%s'",
                        paste(database, table, var, sep = ":"))
        out <- DBI::dbGetQuery(con, sel)
    }
    invisible(out)
}

#' get table names from srmd_repo
#'
#' @return 返回 SRDM 库中的所有数据库、表格和变量名称
#' @export
getallname <- function() {
    database_path <- file.path(Sys.getenv("SRDM_DATA_REPO_PATH"), "srdm_dataRepo.sqlite")
    con <- DBI::dbConnect(RSQLite::SQLite(), database_path)
    on.exit(DBI::dbDisconnect(con))

    varnames <- DBI::dbGetQuery(con, "SELECT name from data_record")$name %>%
        stringr::str_split(":")
    databaseList <- purrr::map_chr(varnames, 1)
    tableList <- purrr::map_chr(varnames, 2)
    varList <- purrr::map_chr(varnames, 3) 
    invisible(data.table(database = databaseList, table = tableList, variable = varList))
}
