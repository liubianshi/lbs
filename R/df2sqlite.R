#' Check attributes of data.frame or vetor
#'
#' @description *Personal use!*
#'
#' @param x a R object for checking
#' @param quietly Bool value, whether output attributes
#'
#' @export
check_attr <- function(x, quietly = FALSE) {
    tab_attr <- c("keys", "source", "description", "script_file",
                  "script_tag", "desc_file", "desc_tag", "log_file")
    var_attr <- c("label", "source", "description", "script_file",
                  "script_tag", "desc_file", "desc_tag", "log_file")

    attr_list <- if (is.data.frame(x)) tab_attr else var_attr
    attr_exist <- vector("character")
    for (a in attr_list) {
        t <- if (!is.null(attr(x, a))) (attr_exist[a] <- attr(x, a)) else ""

        if (!quietly) {
            if ("crayon" %in% rownames(installed.packages()))
                cat(gettextf("  %-15s %-s", a, crayon::underline(t)), "\n")
            else
                cat(gettextf("  %-15s %-s", a, t), "\n")
        }
    }
    invisible(attr_exist)
}

#' Prepare data.frame information for SRDM
#' 
#' @description *Personal use!*. Check whether the data frame meets the
#' requirements of SRDM, and output the basic information of the data to
#' standard output
#' @param df data.frame for archive
#' @param database a string, name of database
#' @param table a string, table name in database
#' @examples
#' \dontrun{
#' df <- mtcars
#' df$ID <- seq_len(nrow(df))
#' attr(df, "keys") = "ID"
#' for (i in seq_along(df))
#'     attr(df[[i]], "label") = paste("label:", names(df)[i])
#' fil <- tempfile("srdm")
#' df_srdm(df, "test", "mtcars", file = fil, replace = TRUE)
#' if (interactive()) file.show(fil)
#' database <- file.path(
#'     ifelse(Sys.getenv("DATA_ARCHIVE") != "",
#'         Sys.getenv("DATA_ARCHIVE"), "~/Data/DBMS"
#'     ), "test.sqlite"
#' )
#' con <- DBI::dbConnect(RSQLite::SQLite(), database)
#' DBI::dbListTables(con)
#' DBI::dbGetQuery(con, "SELECT * FROM mtcars WHERE ID <= 10")
#' DBI::dbDisconnect(con)
#'}
#' @export
df_srdm <- function(df, database, table, file = "", # 
                    replace = FALSE,
                    append = FALSE,
                    field.types = NULL) {
    if (!is.data.frame(df))
        stop("df must be a data frame")
    if (!(length(database) == 1 && stringr::str_detect(database, "^\\w+$")))
        stop("database must be a valid name, match '^\\w+$'")
    if (!(length(table) == 1 && stringr::str_detect(table, "^\\w+$")))
        stop("table must be a valid name, match '^\\w+$'")

    # check the integraty of data frame's attributes
    table_attr <- check_attr(df, quietly = TRUE)
    table_attr["name"] = paste(database, table, sep = ":")
    if (!"keys" %in% names(table_attr))
        stop("Main keys are not setting, try to use attr(df, \"keys\") <-")
    keys <- stringr::str_split(table_attr["keys"], "\\s+")[[1]]
    if (subset(df, select = keys) %>% duplicated() %>% any())
        stop("The main keys cannot meet the uniqueness requirement!")

    # check the integraty of all variables' attributes
    vari_attr  <- lapply(df, check_attr, quietly = TRUE)
    for (i in seq_along(vari_attr)) {
        if (!"label" %in% names(vari_attr[[i]]))
            stop(names(vari_attr)[i], "'s label has not been set.")

        if (( !"source" %in% names(vari_attr[[i]]) ||
               lbs::isempty(vari_attr[[i]]["source"])
            ) && !lbs::isempty(table_attr["source"])) {
            vari_attr[[i]]["source"] <- table_attr["source"]
        }
        vari_attr[[i]]["name"]         <- paste(database, table,
                                                names(vari_attr)[i],
                                                sep = ":")
        vari_attr[[i]]["type"]         <- typeof(df[[i]])
        vari_attr[[i]]["number"]       <- length(df[[i]])
        vari_attr[[i]]["missNumber"]   <- sum(lbs::isempty(df[[i]]))
        vari_attr[[i]]["uniqueNumber"] <- length(unique(df[[i]]))
    }

    # Convert attributes vector to string, and then write it to a file
    srdm_fields <- c(vector2string(table_attr), sapply(vari_attr, vector2string))
    #for (var in names(df)) {
        #var_attr <- check_attr(df[[var]], quietly = TRUE)
        #var_attr["name"] <- paste(database, table, var, sep = ":")
        #srdm_fields <- c(srdm_fields, vector2string(var_attr))
    #}

    if (file == "") {
        if ("crayon" %in% rownames(installed.packages()))
            write(crayon::red$bold("Attributes information:"), "")
        else
            write("Attributes information:", "")

        write(paste0("srdm\t", srdm_fields), "")
        write("----------------------------------------", "")
    } else {
        write("Attributes information:", file)
        write(paste0("srdm\t", srdm_fields), file, append = TRUE)
    }

    # save data to database
    insert_result <- tryCatch(
        df2sqlite(df, database, table, keys, replace, append, field.types),
        error = function(cond) {
            message(paste("File failed to written to", database))
            message("Here's the original error message:")
            message(cond)
            return(NA)
        },
        warning = function(cond) {
            message(paste("Warnings were generated when writing to", database))
            message("Here's the original warning message:")
            message(cond)
            return(NULL)
        },
        finally={
            message(paste("Data has been successfully written to", database))
        }
    )

    invisible(TRUE)
}

# convert a character vector to a string in specific format
vector2string <- function(l) {
    if (length(l) == 0) return(NA)
    content <- vector(mode = "character", length(l))

    for (i in seq_along(l))
        content[i] <- paste(names(l)[i], l[i], sep = "\x02")

    content <- paste(content, collapse = "\x06" )
    invisible(content)
}

#' Write data frame to database
#'
#' @description Writes, replace of append a data frame to a database table. At
#' the same time, setting the primary keys of the table.
#'
#' @param df A data frame of values (or coercible to data.frame).
#' @param database Database name, which will be converted to a database. If
#' environment variable `DATA_ARCHIVE` has been set, then the `database` will
#' be transformed to `$DATA_ARCHIVE/<database>.sqlite`, otherwise, the
#' `database` will be transformed to `$HOME/Data/DBMS/<database>.sqlite.`
#' @param table Table name in the database
#' @param keys character vector, primary keys of data.frame df
#' @param reaplace logical value, whether replace the `table` when it already
#' exists. default: `FALSE`
#' @param append logical value, whether append `df` to the table when it
#' already exists. default: `FALSE`
#' @examples
#' \dontrun{
#' df <- mtcars
#' df$ID <- seq_along(nrows(df))
#' df2sqlite(df, database = "test", table = "mtcars", keys = "ID")
#'
#' df$ID = df$ID + 100
#' try(df2sqlite(df, "test", "mtcars", "ID", append = TRUE))
#' df2sqlite(df, "test", "mtcars", "ID", append = TRUE)
#'
#' df$ID = df$ID + 100
#' df2sqlite(df, "test", "mtcars", "ID", replace = TRUE)
#'}
#' @export
df2sqlite <- function(df, database, table, keys,
                      replace = FALSE, append = FALSE, field.types = NULL ) {
    database <- if (Sys.getenv("DATA_ARCHIVE") != "")
                    file.path(Sys.getenv("DATA_ARCHIVE"), database)
                else
                    file.path(Sys.getenv("HOME"), "Data", "DBMS", database)

    if (!dir.exists(dirname(database)))
        tryCatch(
            dir.create(dirname(database), recursive = TRUE ),
            error = function(cond) {
                message(cond)
                return(FALSE)
            },
            warning = function(cond) {
                message(cond)
                return(FALSE)
            }
        )

    database     <- paste0(database, ".sqlite")
    sth_create   <- gettextf("CREATE TABLE %s (%s, PRIMARY KEY(%s))", table,
                             paste(names(df), collapse = ", "),
                             paste(keys, collapse = ", "))
    sth_back     <- gettextf("ALTER TABLE %s RENAME TO %s_bck", table, table)
    sth_drop_bck <- gettextf("DROP TABLE %s_bck", table)
    sth_drop_new <- gettextf("DROP TABLE %s", table)
    sth_restore  <- gettextf("ALTER TABLE %s_bck RENAME TO %s", table, table)

    con          <- DBI::dbConnect(RSQLite::SQLite(), database)
    table_exists <- table %in% DBI::dbListTables(con)

    if (table_exists) {
        if (!replace && !append)
            stop(paste("Table", table, "is exists!"))
        if (replace) {
            DBI::dbExecute(con, sth_back)
            DBI::dbExecute(con, sth_create)
        }
    } else {
        DBI::dbExecute(con, sth_create)
    }

    tryCatch(
        DBI::dbAppendTable(con, table, df),
        error = function(cond) {
            if (table_exists && replace) {
                DBI::dbExecute(con, sth_drop_new)
                DBI::dbExecute(con, sth_restore)
            }
            if (!table_exists) 
                DBI::dbExecute(con, sth_drop_new)
            message("Data frame failed to written to ", database)
            message("Here's the original error message:")
            message(cond, "\n")
            return(NA)
        },
        warning = function(cond) {
            message(cond)
        },
        finally = {
            if (table_exists && replace)
                DBI::dbExecute(con, sth_drop_bck)
            DBI::dbDisconnect(con)
        }
    )
    invisible(TRUE)
}

