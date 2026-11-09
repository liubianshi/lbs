journal_fixture <- function() {
  GridTable::GridTable(data.frame(
    term = c("Variable", "$x", "", "N"),
    a = c("(1)", "1.2", "(0.3)", "100"),
    b = c("(2)", "2.1", "(0.4)", "100")
  ), header = 1L, footer = 4L)
}

test_that("model names, dictionary fallback and label order are preserved", {
  model <- fixest::feols(log(mpg) ~ wt, data = mtcars)
  expect_equal(get_dependent_variable_names(list(model)), "log(mpg)")
  expect_equal(get_coefficient_names(list(model, model)), c("(Intercept)", "wt"))
  expect_equal(map_dependent_variable_names(c("mpg", "wt"), c(mpg = "Fuel")),
               c("Fuel", "wt"))
  expect_equal(map_dependent_variable_names(c("mpg", "wt"), list(mpg = "Fuel")),
               c("Fuel", "wt"))
  expect_equal(get_variable_labels(c(wt = "Weight", mpg = "Fuel"), c("mpg", "wt")),
               c(mpg = "Fuel", wt = "Weight"))
  expect_equal(get_variable_labels(c(wt = "Weight", mpg = "Fuel"), c("mpg", "wt"),
                                   sort_labels = TRUE), c(wt = "Weight", mpg = "Fuel"))
  expect_error(get_coefficient_names(list(stats::lm(mpg ~ wt, mtcars))))
  expect_equal(translate_table_terms_to_english(c("国家时间", "观测数", NA)),
               c("Country-Year", "Observations", NA))
})

test_that("dependent names use the linear response for fixest IV models", {
  iv <- fixest::feols(mpg ~ wt | hp ~ disp, data = mtcars)
  transformed <- fixest::feols(log(mpg) ~ wt | hp ~ disp, data = mtcars)
  expect_equal(get_dependent_variable_names(list(iv, transformed)), c("mpg", "log(mpg)"))
  expect_equal(map_dependent_variable_names(get_dependent_variable_names(list(iv)),
                                           c(mpg = "Fuel")), "Fuel")
  other <- stats::lm(log(mpg) ~ wt, data = mtcars)
  expect_equal(get_dependent_variable_names(list(other), model_class = "lm"), "log(mpg)")
})

test_that("list dictionaries retain unmatched label positions like character vectors", {
  dictionary <- c(wt = "Weight", hp = "Power")
  for (variables in list(c("wt", "mpg"), c("mpg", "wt", "mpg"),
                         c("mpg", "disp"), character())) {
    expect_identical(get_variable_labels(as.list(dictionary), variables),
                     get_variable_labels(dictionary, variables))
    expect_identical(get_variable_labels(as.list(dictionary), variables, sort_labels = TRUE),
                     get_variable_labels(dictionary, variables, sort_labels = TRUE))
  }
  result <- get_variable_labels(list(wt = "Weight"), c("wt", "mpg"))
  expect_identical(unname(result), c("Weight", NA_character_))
  expect_identical(names(result), c("wt", NA_character_))
})

test_that("kable conversion preserves caption, escapes headers and aligns columns", {
  skip_if_not_installed("GridTable")
  skip_if_not_installed("knitr")
  k <- knitr::kable(data.frame(term = c("Model", "x"), a = c("(1)", "1.2"),
                              b = c("(2)", "2.1")), format = "pipe", caption = "Results")
  x <- enhance_kable_for_gridtable(k, table_label = "tbl:test")
  expect_s3_class(x, "GridTable")
  expect_match(attr(x, "caption"), "{#tbl:test}", fixed = TRUE)
  expect_equal(unname(attr(x, "align")), c("l", "c", "c"))
  expect_equal(x[[2]][2], "\\(1\\)")
  expect_length(attr(x, "merged_cells"), 1L)
  y <- enhance_kable_for_gridtable(k, columns_to_bind = FALSE)
  expect_false(grepl("{#}", attr(y, "caption"), fixed = TRUE))
})

test_that("journal formatting dispatches and merges pairs idempotently", {
  skip_if_not_installed("GridTable")
  x <- journal_fixture()
  world <- format_gtable_by_journal(x)
  expect_identical(world, x)
  expect_equal(x[[1]][2], "\\$x")
  expect_length(attr(x, "merged_cells"), 0L)
  econ <- format_gtable_by_journal(x, journal_style = "economic_research")
  expect_length(attr(econ, "merged_cells"), 2L)
  expect_no_error(format_gtable_by_journal(x, journal_style = "economic_research"))
  expect_length(attr(x, "merged_cells"), 2L)
  expect_error(format_gtable_by_journal(x, journal_style = "invalid"))
  expect_error(format_gtable_by_journal(x, coef_rows = 2L))
  expect_error(format_gtable_by_journal(x, model_cols = 99L))
})

test_that("width adjustment and empty coefficient regions are safe", {
  skip_if_not_installed("GridTable")
  x <- journal_fixture()
  GridTable::merge_cells(x, 1L, 2:3)
  GridTable::set_attr(x, width = c(10, 5, 30))
  expect_no_error(format_gtable_by_journal(x))
  expect_equal(attr(x, "width"), c(10, 5, 6))
  GridTable::set_attr(x, footer = Inf)
  expect_no_error(format_gtable_by_journal(x))
  expect_equal(attr(x, "footer"), 4L)
  GridTable::set_attr(x, footer = 2L)
  expect_no_error(format_gtable_by_journal(x, journal_style = "economic_research"))
  y <- journal_fixture()
  GridTable::merge_cells(y, 2L, 2:3)
  expect_error(format_gtable_by_journal(y, journal_style = "economic_research"), "overlap")
})

test_that("tables without a footer and header-only tables have safe row bounds", {
  skip_if_not_installed("GridTable")
  x <- GridTable::GridTable(data.frame(term = c("Variable", "x", "SE"),
                                      model = c("(1)", "1.2", "(0.3)")), header = 1L)
  expect_no_error(format_gtable_by_journal(x, journal_style = "economic_research"))
  expect_equal(attr(x, "footer"), 4L)
  expect_length(attr(x, "merged_cells"), 1L)
  x <- GridTable::GridTable(data.frame(term = "Variable", model = "(1)"), header = 1L)
  expect_no_error(format_gtable_by_journal(x, journal_style = "economic_research"))
  expect_equal(attr(x, "footer"), 2L)
})
