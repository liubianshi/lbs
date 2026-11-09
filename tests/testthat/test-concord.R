industry_fixture <- function() {
  data.table::data.table(GB11 = c("1011", "1011", "1012"),
                         GB02 = c("0111", "0112", "0111"))
}

test_that("direct concordances preserve weights, names and missing inputs", {
  local_mocked_bindings(get_gbtable = function(orig, dest) {
    x <- industry_fixture()
    x[, c(orig, dest), with = FALSE]
  })
  x <- concord(c("1011", "1012", "1011", NA_character_), "gb11", "gb02", 3L)
  expect_named(x, c("1011", "1012", "1011", NA_character_))
  expect_equal(x[[1]], list(match = "011", weight = 1))
  expect_equal(x[[4]], list(match = NA_character_, weight = NA_real_))
  expect_identical(attr(x, "origin"), "GB11")
  expect_identical(attr(x, "destination"), "GB02")
  expect_warning(y <- concord("9999", "GB11", "GB02"), "9999")
  expect_true(is.na(y[[1]]$match))
  expect_length(concord(character(), "GB11", "GB02"), 0L)
  expect_error(concord(1, "GB11", "GB02"))
  expect_error(concord("1011", "GB11", "GB02", 0))
})

test_that("level and rate indices retain the original two-direction algorithm", {
  local_mocked_bindings(get_gbtable = function(orig, dest) {
    x <- industry_fixture()
    x[, c(orig, dest), with = FALSE]
  })
  data <- data.table::data.table(code = c("0111", "0112"), value = c(100, 200))
  before <- data.table::copy(data)
  expect_equal(cal_index("1011", "GB02", "GB11", data, "rate"), c(`1011` = 150))
  expect_equal(cal_index("1011", "GB02", "GB11", as.data.frame(data)), c(`1011` = 125))
  expect_identical(data, before)
  data$value[2] <- NA_real_
  expect_true(is.na(cal_index("1011", "GB02", "GB11", data)))
  expect_equal(cal_index("1011", "GB02", "GB11", data, na.rm = TRUE), c(`1011` = 25))
  expect_length(cal_index(character(), "GB02", "GB11", data), 0L)
  expect_error(cal_index("1011", "GB02", "GB11", data[c(1, 1)]))
})

test_that("multi-step paths multiply branch weights without renormalization", {
  local_mocked_bindings(get_gbtable = function(orig, dest) {
    if (orig == "GB02") data.table::data.table(GB02 = c("0111", "0111"),
                                              GB11 = c("1011", "1012"))
    else data.table::data.table(GB11 = c("1011", "1012", "1012"),
                               GB17 = c("2011", "2011", "2012"))
  })
  x <- concord("0111", "GB02", "GB17")[[1L]]
  expect_equal(x, list(match = c("2011", "2012"), weight = c(.75, .25)))
  expect_warning(x <- concord("9999", "GB02", "GB17"), "9999")
  expect_true(is.na(x[[1]]$match))
})

test_that("numeric padding handles missing, longer and nonnumeric codes", {
  expect_identical(.concord_format_code(c("111", "1234", "5678", NA, "12345", "AtB")),
                   c("0111", "1234", "5678", NA, "12345", "AtB"))
  expect_identical(.concord_format_code(character()), character())
})

test_that("lake tables reverse only when the forward file is absent", {
  root <- withr::local_tempdir()
  withr::local_envvar(ECON_DATA_LAKE_PATH = root)
  folder <- file.path(root, "raw", "coding_inds", "gb11_gb02@v1")
  dir.create(folder, recursive = TRUE)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  path <- file.path(folder, "data.parquet")
  sql <- paste0("COPY (SELECT '0111' AS GB11_code, '0112' AS GB02_code) TO ",
                 DBI::dbQuoteString(con, path), " (FORMAT PARQUET)")
  DBI::dbExecute(con, sql)
  x <- get_gbtable("gb02", "gb11")
  expect_named(x, c("GB02", "GB11"))
  expect_equal(x$GB02, "0112")
  expect_equal(x$GB11, "0111")
  forward <- file.path(root, "raw", "coding_inds", "gb02_gb11@v1")
  dir.create(forward, recursive = TRUE)
  writeLines("corrupt", file.path(forward, "data.parquet"))
  expect_error(get_gbtable("GB02", "GB11"))
})

test_that("non-GB conversions delegate with all matches", {
  skip_if_not_installed("concordance")
  x <- concord("0101", "HS0", "SITC3", 4L)
  expected <- concordance::concord("0101", "HS0", "SITC3", 4L, all = TRUE)
  attr(expected, "origin") <- "HS0"
  attr(expected, "destination") <- "SITC3"
  expect_equal(x, expected)
})

test_that("missing intermediate branches keep missing mass", {
  local_mocked_bindings(get_gbtable = function(orig, dest) {
    if (orig == "GB02") data.table::data.table(GB02 = c("0111", "0111"),
                                              GB11 = c("1011", "1012"))
    else data.table::data.table(GB11 = "1011", GB17 = "2011")
  })
  expect_warning(x <- concord("0111", "GB02", "GB17"), "1012")
  expect_equal(x[[1]]$match, c("2011", NA_character_))
  expect_equal(x[[1]]$weight, c(.5, NA_real_))
})
