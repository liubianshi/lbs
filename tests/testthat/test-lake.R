# Tests for the data lake R surface in R/lake.R.
# Data-reading functions (lake_read / lake_tables / lake_variables / lake_meta)
# need a populated lake fixture and are only lightly covered here; this file
# focuses on the pure helpers and shell-out-controllable writer side.

# ── lake_dtype ───────────────────────────────────────────────────────────────

test_that("lake_dtype maps R classes to the meta vocabulary", {
    expect_equal(lake_dtype("a"), "string")
    expect_equal(lake_dtype(factor(c("a", "b"))), "string")
    expect_equal(lake_dtype(1L), "integer")
    expect_equal(lake_dtype(1.5), "numeric")
    expect_equal(lake_dtype(TRUE), "boolean")
    expect_equal(lake_dtype(as.Date("2026-01-01")), "date")
    expect_equal(lake_dtype(as.POSIXct("2026-01-01", tz = "UTC")), "timestamp")
})

test_that("lake_dtype returns the original class for unknown types", {
    cplx <- complex(real = 1, imaginary = 1)
    expect_equal(lake_dtype(cplx), "complex")
})

# ── lake_id ──────────────────────────────────────────────────────────────────

test_that("lake_id translates lbs coordinates to a raw entity id", {
    expect_equal(lake_id("CHN_FirmTrade", "HG2005"),     "raw:firmtrade/hg@2005")
    expect_equal(lake_id("CHN_FirmTrade", "HG2005Q4"),   "raw:firmtrade/hg@2005q4")
    expect_equal(lake_id("Coding_Inds", "HS92_ISIC2"),   "raw:coding_inds/hs92_isic2@v1")
})

# ── .parse_entity_id / .lake_entity_dir ──────────────────────────────────────

test_that(".parse_entity_id extracts all four entity types", {
    res <- lbs:::.parse_entity_id("dim:unctad/economy_code@20260601")
    expect_equal(res$type, "dim")
    expect_equal(res$domain, "unctad")
    expect_equal(res$dataset, "economy_code")
    expect_equal(res$version, "20260601")
})

test_that(".parse_entity_id rejects malformed ids", {
    expect_error(lbs:::.parse_entity_id("raw:no_version"), "valid econ-data entity id")
    expect_error(lbs:::.parse_entity_id("bogus:firm/x@v1"), "valid econ-data entity id")
})

test_that(".lake_entity_dir maps an id to its on-disk directory", {
    dir <- lbs:::.lake_entity_dir("raw:firmtrade/hg@2005", "/lake")
    expect_equal(dir, file.path("/lake", "raw", "firmtrade", "hg@2005"))
})

# ── .lake_columns_dt ─────────────────────────────────────────────────────────

test_that(".lake_columns_dt flattens a columns list, NA-ing empty descriptions", {
    cols <- list(
        list(name = "a", dtype = "integer", description = "the id"),
        list(name = "b", dtype = "string",  description = "")
    )
    out <- lbs:::.lake_columns_dt(cols)
    expect_equal(out$name, c("a", "b"))
    expect_equal(out$dtype, c("integer", "string"))
    expect_equal(out$label, c("the id", NA_character_))
})

test_that(".lake_columns_dt returns an empty table for NULL/empty input", {
    out <- lbs:::.lake_columns_dt(NULL)
    expect_equal(nrow(out), 0L)
    expect_named(out, c("name", "dtype", "label"))
})

# ── .parse_raw_id ────────────────────────────────────────────────────────────

test_that(".parse_raw_id extracts source / dataset / version", {
    res <- lbs:::.parse_raw_id("raw:wind/firm_fin@2024q3")
    expect_equal(res$source, "wind")
    expect_equal(res$dataset, "firm_fin")
    expect_equal(res$version, "2024q3")
})

test_that(".parse_raw_id rejects non-raw ids", {
    expect_error(lbs:::.parse_raw_id("dim:firm/cn_a_share@v1"),
                 "raw entities only")
    expect_error(lbs:::.parse_raw_id(""),
                 "raw entities only")
})

test_that(".parse_raw_id points at the offending segment of a malformed id", {
    expect_error(lbs:::.parse_raw_id("raw:no_version"),
                 "exactly one '/'")
    expect_error(lbs:::.parse_raw_id("raw:wind/firm_fin"),
                 "exactly one '@'")
    expect_error(lbs:::.parse_raw_id("raw:Wind/firm_fin@2024q3"),
                 "invalid source segment")
    expect_error(lbs:::.parse_raw_id("raw:wind/firm_fin@2024 q3"),
                 "invalid version segment")
})

# ── .lake_iso_now ────────────────────────────────────────────────────────────

test_that(".lake_iso_now emits RFC3339 with Z suffix (not '+0800')", {
    ts <- lbs:::.lake_iso_now()
    expect_match(ts, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$")
})

# ── .build_lake_columns ──────────────────────────────────────────────────────

test_that(".build_lake_columns auto-fills TODO when no override given", {
    df <- data.frame(a = 1L, b = "x", stringsAsFactors = FALSE)
    out <- lbs:::.build_lake_columns(df, columns = NULL)
    expect_length(out, 2L)
    expect_equal(out[[1]]$name, "a")
    expect_equal(out[[1]]$dtype, "integer")
    expect_equal(out[[1]]$description, "TODO")
})

test_that(".build_lake_columns accepts a character description override", {
    df <- data.frame(a = 1L, b = "x", stringsAsFactors = FALSE)
    out <- lbs:::.build_lake_columns(df, columns = list(a = "the id"))
    expect_equal(out[[1]]$description, "the id")
    expect_equal(out[[2]]$description, "TODO")
})

test_that(".build_lake_columns accepts list override with dtype + description", {
    df <- data.frame(a = 1L, stringsAsFactors = FALSE)
    out <- lbs:::.build_lake_columns(df,
        columns = list(a = list(description = "code", dtype = "string")))
    expect_equal(out[[1]]$dtype, "string")
    expect_equal(out[[1]]$description, "code")
})

test_that(".build_lake_columns rejects unsupported override shapes", {
    df <- data.frame(a = 1L, stringsAsFactors = FALSE)
    expect_error(
        lbs:::.build_lake_columns(df, columns = list(a = 1.5)),
        "must be a character description"
    )
})

# ── lake_register (dry-run) ──────────────────────────────────────────────────

test_that("lake_register(dry_run = TRUE) returns a plan without touching disk", {
    tmp <- withr::local_tempdir()
    withr::local_envvar(ECON_DATA_LAKE_PATH = tmp)

    df <- data.frame(firm_id = 1L, year = 2020L, revenue = 1.5)
    res <- lake_register(df, id = "raw:demo/foo@v1",
                         columns = list(firm_id = "企业ID"),
                         dry_run = TRUE)

    expect_true(res$dry_run)
    expect_equal(res$id, "raw:demo/foo@v1")
    expect_match(res$data_path, file.path(tmp, "raw", "demo", "foo@v1", "data.parquet"),
                 fixed = TRUE)
    # No files written.
    expect_false(file.exists(res$data_path))
    expect_false(file.exists(res$meta_path))

    # Meta carries the user-supplied description plus TODO fallbacks.
    desc_by_name <- vapply(res$meta$columns,
                           function(c) c$description %||% NA_character_,
                           character(1))
    names(desc_by_name) <- vapply(res$meta$columns, function(c) c$name, character(1))
    expect_equal(desc_by_name[["firm_id"]], "企业ID")
    expect_equal(desc_by_name[["year"]],    "TODO")
})

test_that("lake_register validates df is a data.frame", {
    expect_error(lake_register(c(1, 2, 3), "raw:demo/x@v1", dry_run = TRUE),
                 "must be a data.frame")
})

test_that("lake_register validates df has at least one column", {
    expect_error(
        lake_register(data.frame()[0, , drop = FALSE],
                      "raw:demo/x@v1", dry_run = TRUE),
        "zero columns"
    )
})

test_that("lake_register without CLI on PATH fails with a clear message", {
    tmp <- withr::local_tempdir()
    withr::local_envvar(ECON_DATA_LAKE_PATH = tmp, PATH = "/nonexistent")

    df <- data.frame(a = 1L)
    expect_error(
        lake_register(df, "raw:demo/x@v1",
                      econdata_bin = "/nonexistent/econ-data"),
        "econ-data CLI not found"
    )
})

# ── lake_write ───────────────────────────────────────────────────────────────

test_that("lake_write round-trips a simple data frame", {
    tmp <- withr::local_tempdir()
    path <- file.path(tmp, "out.parquet")
    df <- data.frame(a = 1:3, b = letters[1:3], stringsAsFactors = FALSE)

    res <- lake_write(df, path)
    expect_equal(res, path)
    expect_true(file.exists(path))

    back <- as.data.frame(arrow::read_parquet(path))
    expect_equal(back$a, df$a)
    expect_equal(back$b, df$b)
})

test_that("lake_write warns when a list column would later be rejected by CLI", {
    tmp <- withr::local_tempdir()
    path <- file.path(tmp, "out.parquet")
    df <- data.frame(a = 1:2)
    df$payload <- list(list(x = 1), list(x = 2))  # list-column

    expect_warning(lake_write(df, path), "econ-data CLI will reject")
})
