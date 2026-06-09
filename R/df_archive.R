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
      cat(gettextf("  %-15s %-s", a, t), "\n")
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
  stopifnot(anyDuplicated(data.table::setDT(df)[, ..keys]) == 0)

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
#' @param ... arguments forwarded to `df_archive()`.
#' @export
df_srdm <- function(...) {
  .Deprecated("df_archive")
  df_archive(...)
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

parquet_path <- function(database, table) {
  file.path(archive_base_dir(), paste0(database, "_", table, ".parquet"))
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
#' @export
df2parquet <- function(df, database, table, replace = FALSE, append = FALSE) {
  path <- parquet_path(database, table)
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)

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
