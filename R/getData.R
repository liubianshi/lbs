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
                       condition = NULL, and = TRUE, limit = NULL) {
    con <- DBI::dbConnect(RSQLite::SQLite(), database)
    on.exit(dbDisconnect(con))

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
    out <- DBI::dbGetQuery(con, sel)
    DBI::dbDisconnect(con)

    invisible(out)
}
