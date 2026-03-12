# Helper: create a minimal labelled data frame for testing
make_test_df <- function(n = 5) {
    df <- data.frame(
        id  = seq_len(n),
        val = as.numeric(seq_len(n)) * 1.1,
        stringsAsFactors = FALSE
    )
    attr(df, "keys")   <- "id"
    attr(df, "source") <- "test_source"
    attr(df$id,  "label") <- "Record ID"
    attr(df$val, "label") <- "Numeric Value"
    df
}

# ── repo_parquet_path ──────────────────────────────────────────────────────────

test_that("repo_parquet_path constructs correct paths", {
    repo_dir <- withr::local_tempdir()
    withr::local_envvar(SRDM_DATA_REPO_PATH = repo_dir)

    expect_equal(
        lbs:::repo_parquet_path("data_table"),
        file.path(repo_dir, "srdm_data_table.parquet")
    )
    expect_equal(
        lbs:::repo_parquet_path("data_record"),
        file.path(repo_dir, "srdm_data_record.parquet")
    )
})

# ── upsert_parquet_row ────────────────────────────────────────────────────────

test_that("upsert_parquet_row creates new file on first write", {
    repo_dir <- withr::local_tempdir()
    path <- file.path(repo_dir, "test.parquet")
    row  <- data.frame(name = "a", val = 1L, stringsAsFactors = FALSE)

    result <- lbs:::upsert_parquet_row(path, row)
    expect_true(isTRUE(result))
    expect_true(file.exists(path))
    out <- arrow::read_parquet(path)
    expect_equal(nrow(out), 1L)
    expect_equal(out$name, "a")
})

test_that("upsert_parquet_row appends row with new key", {
    repo_dir <- withr::local_tempdir()
    path <- file.path(repo_dir, "test.parquet")
    row1 <- data.frame(name = "a", val = 1L, stringsAsFactors = FALSE)
    row2 <- data.frame(name = "b", val = 2L, stringsAsFactors = FALSE)

    lbs:::upsert_parquet_row(path, row1)
    lbs:::upsert_parquet_row(path, row2)
    out <- arrow::read_parquet(path)
    expect_equal(nrow(out), 2L)
    expect_setequal(out$name, c("a", "b"))
})

test_that("upsert_parquet_row returns FALSE and keeps old value when replace=FALSE", {
    repo_dir <- withr::local_tempdir()
    path <- file.path(repo_dir, "test.parquet")
    row1 <- data.frame(name = "a", val = 1L, stringsAsFactors = FALSE)
    row2 <- data.frame(name = "a", val = 99L, stringsAsFactors = FALSE)

    lbs:::upsert_parquet_row(path, row1)
    result <- lbs:::upsert_parquet_row(path, row2, replace = FALSE)
    expect_false(isTRUE(result))
    out <- arrow::read_parquet(path)
    expect_equal(nrow(out), 1L)
    expect_equal(out$val, 1L)
})

test_that("upsert_parquet_row replaces existing row when replace=TRUE", {
    repo_dir <- withr::local_tempdir()
    path <- file.path(repo_dir, "test.parquet")
    row1 <- data.frame(name = "a", val = 1L, stringsAsFactors = FALSE)
    row2 <- data.frame(name = "a", val = 99L, stringsAsFactors = FALSE)

    lbs:::upsert_parquet_row(path, row1)
    lbs:::upsert_parquet_row(path, row2, replace = TRUE)
    out <- arrow::read_parquet(path)
    expect_equal(nrow(out), 1L)
    expect_equal(out$val, 99L)
})

# ── write_repo_direct ─────────────────────────────────────────────────────────

test_that("write_repo_direct creates both parquet metadata files with correct columns", {
    repo_dir <- withr::local_tempdir()
    withr::local_envvar(SRDM_DATA_REPO_PATH = repo_dir)

    df <- make_test_df()
    table_attr <- check_attr(df, quietly = TRUE)
    table_attr["name"] <- "test:mtcars"
    vari_attr <- lapply(df, check_attr, quietly = TRUE)
    for (i in seq_along(vari_attr)) {
        vari_attr[[i]]["name"]         <- paste("test", "mtcars", names(vari_attr)[i], sep = ":")
        vari_attr[[i]]["type"]         <- typeof(df[[i]])
        vari_attr[[i]]["number"]       <- length(df[[i]])
        vari_attr[[i]]["missNumber"]   <- 0L
        vari_attr[[i]]["uniqueNumber"] <- length(unique(df[[i]]))
    }

    lbs:::write_repo_direct(table_attr, vari_attr, path = "/tmp/fake.parquet")

    tbl_path <- lbs:::repo_parquet_path("data_table")
    rec_path <- lbs:::repo_parquet_path("data_record")
    expect_true(file.exists(tbl_path))
    expect_true(file.exists(rec_path))

    tbl <- arrow::read_parquet(tbl_path)
    rec <- arrow::read_parquet(rec_path)

    # table row
    expect_equal(nrow(tbl), 1L)
    expect_equal(tbl$name, "test:mtcars")
    expect_equal(tbl$engine, "Parquet")
    expect_true(all(c("name", "keys", "path", "engine", "source") %in% names(tbl)))

    # record rows: one per column
    expect_equal(nrow(rec), ncol(df))
    expect_true(all(c("name", "type", "label", "number", "missNumber", "uniqueNumber") %in% names(rec)))
    expect_true(all(grepl("^test:mtcars:", rec$name)))
})

# ── df_srdm full pipeline ─────────────────────────────────────────────────────

test_that("df_srdm writes data parquet and metadata parquet files", {
    data_dir <- withr::local_tempdir()
    repo_dir <- withr::local_tempdir()
    withr::local_envvar(DATA_ARCHIVE = data_dir, SRDM_DATA_REPO_PATH = repo_dir)

    df <- make_test_df()
    expect_invisible(df_srdm(df, "test", "mtcars", replace = TRUE))

    data_path <- file.path(data_dir, "test_mtcars.parquet")
    expect_true(file.exists(data_path))
    expect_true(file.exists(lbs:::repo_parquet_path("data_table")))
    expect_true(file.exists(lbs:::repo_parquet_path("data_record")))

    tbl <- arrow::read_parquet(lbs:::repo_parquet_path("data_table"))
    expect_equal(tbl$name, "test:mtcars")
    rec <- arrow::read_parquet(lbs:::repo_parquet_path("data_record"))
    expect_equal(nrow(rec), ncol(df))
})

test_that("df_srdm append mode does not duplicate metadata rows", {
    data_dir <- withr::local_tempdir()
    repo_dir <- withr::local_tempdir()
    withr::local_envvar(DATA_ARCHIVE = data_dir, SRDM_DATA_REPO_PATH = repo_dir)

    df1 <- make_test_df(5)
    df2 <- make_test_df(3)
    df2$id <- df2$id + 100L  # non-overlapping keys

    df_srdm(df1, "test", "mtcars", replace = TRUE)
    df_srdm(df2, "test", "mtcars", append = TRUE)

    tbl <- arrow::read_parquet(lbs:::repo_parquet_path("data_table"))
    expect_equal(nrow(tbl), 1L)  # exactly one table entry

    rec <- arrow::read_parquet(lbs:::repo_parquet_path("data_record"))
    expect_equal(nrow(rec), ncol(df1))  # no duplicate variable rows
})

# ── getdatainfo / getallvar / getalltable ─────────────────────────────────────

test_that("getdatainfo returns variable info by name", {
    data_dir <- withr::local_tempdir()
    repo_dir <- withr::local_tempdir()
    withr::local_envvar(DATA_ARCHIVE = data_dir, SRDM_DATA_REPO_PATH = repo_dir)

    df <- make_test_df()
    df_srdm(df, "test", "tbl1", replace = TRUE)

    info <- getdatainfo("test", "tbl1", c("id", "val"))
    expect_s3_class(info, "data.table")
    expect_equal(nrow(info), 2L)
    expect_true("label" %in% names(info))
    expect_equal(info$label, c("Record ID", "Numeric Value"))
})

test_that("getdatainfo with var=NULL returns table-level info", {
    data_dir <- withr::local_tempdir()
    repo_dir <- withr::local_tempdir()
    withr::local_envvar(DATA_ARCHIVE = data_dir, SRDM_DATA_REPO_PATH = repo_dir)

    df <- make_test_df()
    df_srdm(df, "test", "tbl2", replace = TRUE)

    info <- getdatainfo("test", "tbl2")
    expect_s3_class(info, "data.table")
    expect_true("name" %in% names(info))
    expect_equal(info$name, "test:tbl2")
})

test_that("getallvar lists all variables from repo", {
    data_dir <- withr::local_tempdir()
    repo_dir <- withr::local_tempdir()
    withr::local_envvar(DATA_ARCHIVE = data_dir, SRDM_DATA_REPO_PATH = repo_dir)

    df <- make_test_df()
    df_srdm(df, "test", "tbl3", replace = TRUE)

    vars <- getallvar()
    expect_s3_class(vars, "data.table")
    expect_true(all(c("database", "table", "variable", "label") %in% names(vars)))
    tbl3_rows <- vars[vars$database == "test" & vars$table == "tbl3", ]
    expect_equal(nrow(tbl3_rows), ncol(df))
})

test_that("getalltable lists all tables from repo", {
    data_dir <- withr::local_tempdir()
    repo_dir <- withr::local_tempdir()
    withr::local_envvar(DATA_ARCHIVE = data_dir, SRDM_DATA_REPO_PATH = repo_dir)

    df <- make_test_df()
    df_srdm(df, "test", "tbl4", replace = TRUE)

    tables <- getalltable()
    expect_s3_class(tables, "data.table")
    expect_true(all(c("database", "table") %in% names(tables)))
    expect_true(any(tables$database == "test" & tables$table == "tbl4"))
})
