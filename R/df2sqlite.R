#' Check attributes of data.frame or vetor
#'
#' @description *Personal use!*
#'
#' @param x a R object for checking
#' @param quietly Bool value, whether output attributes
#'
#' @export
check_attr <- function(x, quietly = FALSE) {
  tab_attr <- c(
    "keys",
    "source",
    "description",
    "script_file",
    "script_tag",
    "desc_file",
    "desc_tag",
    "log_file"
  )
  var_attr <- c(
    "label",
    "source",
    "description",
    "script_file",
    "script_tag",
    "desc_file",
    "desc_tag",
    "log_file"
  )
  attr_list <- if (is.data.frame(x)) tab_attr else var_attr

  attr_exist <- vector("character")
  for (a in attr_list) {
    t <- if (!is.null(attr(x, a))) (attr_exist[a] <- attr(x, a)) else ""

    if (!quietly) {
      if ("crayon" %in% rownames(installed.packages())) {
        cat(gettextf("  %-15s %-s", a, crayon::underline(t)), "\n")
      } else {
        cat(gettextf("  %-15s %-s", a, t), "\n")
      }
    }
  }
  invisible(attr_exist)
}

#' Archive a data frame to Parquet and update the metadata index
#'
#' @description *Personal use!* Write a labelled data frame to a Parquet file
#'   under `$DATA_ARCHIVE` and record its metadata in `_meta_tables.parquet`
#'   and `_meta_variables.parquet`.
#' @param df A data.frame with `keys` attribute set and all columns labelled.
#' @param database Database name (alphanumeric/underscore only).
#' @param table Table name (alphanumeric/underscore only).
#' @param replace Logical. Overwrite existing file/metadata. Default FALSE.
#' @param append Logical. Append rows to existing file. Default FALSE.
#' @param write_repo Logical. Update metadata index. Default TRUE.
#' @return TRUE invisibly.
#' @export
df_archive <- function(
  df,
  database,
  table,
  replace = FALSE,
  append = FALSE,
  write_repo = TRUE
) {
  stopifnot(is.data.frame(df))
  stopifnot(length(database) == 1 && stringr::str_detect(database, "^\\w+$"))
  stopifnot(length(table) == 1 && stringr::str_detect(table, "^\\w+$"))

  # Transform date to character
  for (i in seq_along(df)) {
    if (any(class(df[[i]]) %in% c("Date", "POSIXt"))) {
      df[[i]] <- as.character(as.POSIXct(df[[i]]))
    }
  }

  # check the integraty of data frame's attributes
  table_attr <- check_attr(df, quietly = TRUE)
  table_attr["name"] <- paste(database, table, sep = ":")
  stopifnot("keys" %in% names(table_attr))
  keys <- stringr::str_split(table_attr["keys"], "\\s+")[[1]]
  stopifnot(anyDuplicated(setDT(df)[, ..keys]) == 0)

  # check the integraty of all variables' attributes
  vari_attr <- lapply(df, check_attr, quietly = TRUE)
  for (i in seq_along(vari_attr)) {
    stopifnot("label" %in% names(vari_attr[[i]]))

    if (
      (!"source" %in% names(vari_attr[[i]]) ||
        isempty(vari_attr[[i]]["source"])) &&
        !isempty(table_attr["source"])
    ) {
      vari_attr[[i]]["source"] <- table_attr["source"]
    }
    vari_attr[[i]]["name"] <- paste(
      database,
      table,
      names(vari_attr)[i],
      sep = ":"
    )
    vari_attr[[i]]["type"] <- typeof(df[[i]])
    vari_attr[[i]]["number"] <- length(df[[i]])
    vari_attr[[i]]["missNumber"] <- sum(isempty(df[[i]]))
    vari_attr[[i]]["uniqueNumber"] <- length(unique(df[[i]]))
  }

  message("Began writing data to database")
  insert_result <- tryCatch(
    df2parquet(df, database, table, replace, append),
    error = function(cond) {
      message(paste("File failed to written to", database))
      message("Here's the original error message:")
      stop(cond)
    }
  )
  message("Data Written Successfully!")

  if (isTRUE(write_repo && insert_result)) {
    write_repo_direct(
      table_attr,
      vari_attr,
      path = parquet_path(database, table),
      replace = replace
    )
  }
  invisible(TRUE)
}

#' @rdname df_archive
#' @export
df_srdm <- function(...) {
  .Deprecated("df_archive")
  df_archive(...)
}

meta_parquet_path <- function(tbl) {
  base <- if (nzchar(Sys.getenv("DATA_ARCHIVE"))) {
    Sys.getenv("DATA_ARCHIVE")
  } else {
    file.path(Sys.getenv("HOME"), "Data", "DBMS")
  }
  filename <- switch(tbl,
    tables    = "_meta_tables.parquet",
    variables = "_meta_variables.parquet",
    stop("unknown meta table: ", tbl)
  )
  file.path(base, filename)
}

upsert_parquet_row <- function(path, new_row, replace = FALSE) {
  if (file.exists(path)) {
    existing <- arrow::read_parquet(path)
    key_val <- new_row[["name"]]
    exists <- key_val %in% existing[["name"]]
    if (exists) {
      if (!replace) {
        return(invisible(FALSE))
      }
      existing <- existing[existing[["name"]] != key_val, ]
    }
    combined <- rbind(existing, new_row)
  } else {
    combined <- new_row
  }
  arrow::write_parquet(combined, path)
  invisible(TRUE)
}

write_repo_direct <- function(table_attr, vari_attr, path, replace = FALSE) {
  attr_val <- function(v, key) {
    if (key %in% names(v)) v[[key]] else NA_character_
  }

  tbl_row <- data.frame(
    name = table_attr["name"],
    keys = table_attr["keys"],
    path = path,
    engine = "Parquet",
    source = attr_val(table_attr, "source"),
    description = attr_val(table_attr, "description"),
    script_file = attr_val(table_attr, "script_file"),
    script_tag = attr_val(table_attr, "script_tag"),
    desc_file = attr_val(table_attr, "desc_file"),
    desc_tag = attr_val(table_attr, "desc_tag"),
    log_file = attr_val(table_attr, "log_file"),
    stringsAsFactors = FALSE
  )
  upsert_parquet_row(
    meta_parquet_path("tables"),
    tbl_row,
    replace = replace
  )

  rec_path <- meta_parquet_path("variables")
  for (va in vari_attr) {
    rec_row <- data.frame(
      name = va["name"],
      type = va["type"],
      source = if ("source" %in% names(va)) va["source"] else "unknown",
      label = va["label"],
      description = attr_val(va, "description"),
      number = as.integer(va["number"]),
      missNumber = as.integer(va["missNumber"]),
      uniqueNumber = as.integer(va["uniqueNumber"]),
      script_file = attr_val(va, "script_file"),
      script_tag = attr_val(va, "script_tag"),
      desc_file = attr_val(va, "desc_file"),
      desc_tag = attr_val(va, "desc_tag"),
      log_file = attr_val(va, "log_file"),
      stringsAsFactors = FALSE
    )
    upsert_parquet_row(rec_path, rec_row, replace = replace)
  }
  invisible(TRUE)
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
df2sqlite <- function(
  df,
  database,
  table,
  keys,
  replace = FALSE,
  append = FALSE
) {
  # 生成数据库文件
  database <- if (Sys.getenv("DATA_ARCHIVE") != "") {
    file.path(Sys.getenv("DATA_ARCHIVE"), database)
  } else {
    file.path(Sys.getenv("HOME"), "Data", "DBMS", database)
  }

  # 在文件夹不存在的情况下创建新的文件夹
  if (!dir.exists(dirname(database))) {
    tryCatch(
      dir.create(dirname(database), recursive = TRUE),
      error = function(cond) {
        message(cond)
        return(FALSE)
      },
      warning = function(cond) {
        message(cond)
        return(FALSE)
      }
    )
  }

  stopifnot(length(database) == 1 && length(table) == 1)
  database <- paste0(database, ".sqlite")
  sth_create <- gettextf(
    "CREATE TABLE %s (%s, PRIMARY KEY(%s))",
    table,
    paste(dfname2sql(df), collapse = ", "),
    paste(keys, collapse = ", ")
  )
  sth_back <- gettextf("ALTER TABLE %s RENAME TO %s_bck", table, table)
  sth_drop_bck <- gettextf("DROP TABLE %s_bck", table)
  sth_drop_new <- gettextf("DROP TABLE %s", table)
  sth_restore <- gettextf("ALTER TABLE %s_bck RENAME TO %s", table, table)

  con <- DBI::dbConnect(RSQLite::SQLite(), database)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  table_exists <- toupper(table) %in% toupper(DBI::dbListTables(con))
  if (table_exists) {
    if (!replace && !append) {
      return(NA)
    }
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
      if (!table_exists) {
        DBI::dbExecute(con, sth_drop_new)
      }
      message("Data frame failed to written to ", database)
      message("Here's the original error message:")
      stop(cond, "\n")
    }
  )

  if (table_exists && replace) {
    DBI::dbExecute(con, sth_drop_bck)
  }
  message("Data frame has been written successfully")
  invisible(TRUE)
}

# Helper: resolve parquet file path for a given database+table
parquet_path <- function(database, table) {
  base <- if (Sys.getenv("DATA_ARCHIVE") != "") {
    Sys.getenv("DATA_ARCHIVE")
  } else {
    file.path(Sys.getenv("HOME"), "Data", "DBMS")
  }
  file.path(base, paste0(database, "_", table, ".parquet"))
}

#' Write data frame to Parquet file
#'
#' @description Writes, replaces or appends a data frame to a Parquet file.
#' File path: `$DATA_ARCHIVE/<database>_<table>.parquet`
#'
#' @param df A data frame of values.
#' @param database Database name.
#' @param table Table name.
#' @param replace logical. If TRUE, overwrite existing file. Default FALSE.
#' @param append logical. If TRUE, read existing file, rbind and rewrite. Default FALSE.
#' @return TRUE invisibly on success, NA if file exists and neither replace nor append.
#' @importFrom arrow read_parquet write_parquet
#' @export
df2parquet <- function(df, database, table, replace = FALSE, append = FALSE) {
  path <- parquet_path(database, table)

  if (!dir.exists(dirname(path))) {
    tryCatch(
      dir.create(dirname(path), recursive = TRUE),
      error = function(cond) {
        message(cond)
        return(FALSE)
      },
      warning = function(cond) {
        message(cond)
        return(FALSE)
      }
    )
  }

  file_exists <- file.exists(path)

  if (file_exists) {
    if (!replace && !append) {
      return(NA)
    }
    if (append) {
      existing <- arrow::read_parquet(path)
      df <- rbind(existing, df)
    }
    # replace: just overwrite; append: write merged data
  }

  arrow::write_parquet(df, path)
  message("Data frame has been written successfully to ", path)
  invisible(TRUE)
}

dfname2sql <- function(df) {
  name2sql <- function(name) {
    if (is.list(df[[name]])) {
      paste(name, "BLOB")
    } else if (is.integer(df[[name]])) {
      paste(name, "INTEGER")
    } else if (is.numeric(df[[name]])) {
      paste(name, "NUMERIC")
    } else if (is.character(df[[name]])) {
      paste(name, "TEXT")
    } else {
      paste(name, "NONE")
    }
  }
  purrr::map_chr(names(df), name2sql)
}
