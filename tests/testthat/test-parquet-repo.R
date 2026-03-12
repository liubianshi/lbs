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

# ── meta_parquet_path ─────────────────────────────────────────────────────────

test_that("meta_parquet_path constructs correct paths", {
    data_dir <- withr::local_tempdir()
    withr::local_envvar(DATA_ARCHIVE = data_dir)

    expect_equal(
        lbs:::meta_parquet_path("tables"),
        file.path(data_dir, "_meta_tables.parquet")
    )
    expect_equal(
        lbs:::meta_parquet_path("variables"),
        file.path(data_dir, "_meta_variables.parquet")
    )
    expect_error(lbs:::meta_parquet_path("unknown"))
})

# ── upsert_parquet_row ────────────────────────────────────────────────────────

test_that("upsert_parquet_row creates new file on first write", {
    tmp  <- withr::local_tempdir()
    path <- file.path(tmp, "test.parquet")
    row  <- data.frame(name = "a", val = 1L, stringsAsFactors = FALSE)

    result <- lbs:::upsert_parquet_row(path, row)
    expect_true(isTRUE(result))
    expect_true(file.exists(path))
    out <- arrow::read_parquet(path)
    expect_equal(nrow(out), 1L)
    expect_equal(out$name, "a")
})

test_that("upsert_parquet_row appends row with new key", {
    tmp  <- withr::local_tempdir()
    path <- file.path(tmp, "test.parquet")
    row1 <- data.frame(name = "a", val = 1L, stringsAsFactors = FALSE)
    row2 <- data.frame(name = "b", val = 2L, stringsAsFactors = FALSE)

    lbs:::upsert_parquet_row(path, row1)
    lbs:::upsert_parquet_row(path, row2)
    out <- arrow::read_parquet(path)
    expect_equal(nrow(out), 2L)
    expect_setequal(out$name, c("a", "b"))
})

test_that("upsert_parquet_row returns FALSE and keeps old value when replace=FALSE", {
    tmp  <- withr::local_tempdir()
    path <- file.path(tmp, "test.parquet")
    row1 <- data.frame(name = "a", val = 1L,  stringsAsFactors = FALSE)
    row2 <- data.frame(name = "a", val = 99L, stringsAsFactors = FALSE)

    lbs:::upsert_parquet_row(path, row1)
    result <- lbs:::upsert_parquet_row(path, row2, replace = FALSE)
    expect_false(isTRUE(result))
    out <- arrow::read_parquet(path)
    expect_equal(nrow(out), 1L)
    expect_equal(out$val, 1L)
})

test_that("upsert_parquet_row replaces existing row when replace=TRUE", {
    tmp  <- withr::local_tempdir()
    path <- file.path(tmp, "test.parquet")
    row1 <- data.frame(name = "a", val = 1L,  stringsAsFactors = FALSE)
    row2 <- data.frame(name = "a", val = 99L, stringsAsFactors = FALSE)

    lbs:::upsert_parquet_row(path, row1)
    lbs:::upsert_parquet_row(path, row2, replace = TRUE)
    out <- arrow::read_parquet(path)
    expect_equal(nrow(out), 1L)
    expect_equal(out$val, 99L)
})

# ── write_repo_direct ─────────────────────────────────────────────────────────

test_that("write_repo_direct creates both metadata files with correct columns", {
    data_dir <- withr::local_tempdir()
    withr::local_envvar(DATA_ARCHIVE = data_dir)

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

    tbl_path <- lbs:::meta_parquet_path("tables")
    var_path <- lbs:::meta_parquet_path("variables")
    expect_true(file.exists(tbl_path))
    expect_true(file.exists(var_path))

    tbl <- arrow::read_parquet(tbl_path)
    rec <- arrow::read_parquet(var_path)

    expect_equal(nrow(tbl), 1L)
    expect_equal(tbl$name, "test:mtcars")
    expect_equal(tbl$engine, "Parquet")
    expect_true(all(c("name", "keys", "path", "engine", "source") %in% names(tbl)))

    expect_equal(nrow(rec), ncol(df))
    expect_true(all(c("name", "type", "label", "number", "missNumber", "uniqueNumber") %in% names(rec)))
    expect_true(all(grepl("^test:mtcars:", rec$name)))
})

# ── df_archive full pipeline ──────────────────────────────────────────────────

test_that("df_archive writes data and metadata parquet files", {
    data_dir <- withr::local_tempdir()
    withr::local_envvar(DATA_ARCHIVE = data_dir)

    df <- make_test_df()
    expect_invisible(df_archive(df, "test", "mtcars", replace = TRUE))

    expect_true(file.exists(file.path(data_dir, "test_mtcars.parquet")))
    expect_true(file.exists(lbs:::meta_parquet_path("tables")))
    expect_true(file.exists(lbs:::meta_parquet_path("variables")))

    tbl <- arrow::read_parquet(lbs:::meta_parquet_path("tables"))
    expect_equal(tbl$name, "test:mtcars")
    rec <- arrow::read_parquet(lbs:::meta_parquet_path("variables"))
    expect_equal(nrow(rec), ncol(df))
})

test_that("df_archive append mode does not duplicate metadata rows", {
    data_dir <- withr::local_tempdir()
    withr::local_envvar(DATA_ARCHIVE = data_dir)

    df1 <- make_test_df(5)
    df2 <- make_test_df(3)
    df2$id <- df2$id + 100L

    df_archive(df1, "test", "mtcars", replace = TRUE)
    df_archive(df2, "test", "mtcars", append = TRUE)

    tbl <- arrow::read_parquet(lbs:::meta_parquet_path("tables"))
    expect_equal(nrow(tbl), 1L)

    rec <- arrow::read_parquet(lbs:::meta_parquet_path("variables"))
    expect_equal(nrow(rec), ncol(df1))
})

test_that("df_srdm is deprecated but still works", {
    data_dir <- withr::local_tempdir()
    withr::local_envvar(DATA_ARCHIVE = data_dir)

    df <- make_test_df()
    expect_warning(df_srdm(df, "test", "mtcars", replace = TRUE), "deprecated")
    expect_true(file.exists(file.path(data_dir, "test_mtcars.parquet")))
})

# ── getdatainfo / getallvar / getalltable ─────────────────────────────────────

test_that("getdatainfo returns variable info by name", {
    data_dir <- withr::local_tempdir()
    withr::local_envvar(DATA_ARCHIVE = data_dir)

    df <- make_test_df()
    df_archive(df, "test", "tbl1", replace = TRUE)

    info <- getdatainfo("test", "tbl1", c("id", "val"))
    expect_s3_class(info, "data.table")
    expect_equal(nrow(info), 2L)
    expect_equal(info$label, c("Record ID", "Numeric Value"))
})

test_that("getdatainfo with var=NULL returns table-level info", {
    data_dir <- withr::local_tempdir()
    withr::local_envvar(DATA_ARCHIVE = data_dir)

    df <- make_test_df()
    df_archive(df, "test", "tbl2", replace = TRUE)

    info <- getdatainfo("test", "tbl2")
    expect_s3_class(info, "data.table")
    expect_equal(info$name, "test:tbl2")
})

test_that("list_variables lists all variables", {
    data_dir <- withr::local_tempdir()
    withr::local_envvar(DATA_ARCHIVE = data_dir)

    df <- make_test_df()
    df_archive(df, "test", "tbl3", replace = TRUE)

    vars <- list_variables()
    expect_s3_class(vars, "data.table")
    expect_true(all(c("database", "table", "variable", "label") %in% names(vars)))
    expect_equal(nrow(vars[database == "test" & table == "tbl3"]), ncol(df))
})

test_that("list_tables lists all tables", {
    data_dir <- withr::local_tempdir()
    withr::local_envvar(DATA_ARCHIVE = data_dir)

    df <- make_test_df()
    df_archive(df, "test", "tbl4", replace = TRUE)

    tables <- list_tables()
    expect_s3_class(tables, "data.table")
    expect_true(all(c("database", "table") %in% names(tables)))
    expect_true(any(tables$database == "test" & tables$table == "tbl4"))
})

test_that("getallvar and getalltable are deprecated but still work", {
    data_dir <- withr::local_tempdir()
    withr::local_envvar(DATA_ARCHIVE = data_dir)

    df <- make_test_df()
    df_archive(df, "test", "tbl5", replace = TRUE)

    expect_warning(getallvar(), "deprecated")
    expect_warning(getalltable(), "deprecated")
})
