test_that("logger_factory returns a logger object with the expected methods", {
  log <- logger_factory("Test")
  expect_s3_class(log, "logger")
  expect_named(log, c("info", "warning", "error", "debug"))
  for (fn in log) expect_type(fn, "closure")
})

test_that("logger_factory rejects invalid prefix input", {
  expect_error(logger_factory(123),             "single character string")
  expect_error(logger_factory(c("a", "b")),   "single character string")
  expect_error(logger_factory(character(0)),    "single character string")
  expect_error(logger_factory(NULL),            "single character string")
})

test_that("logger_factory rejects invalid debug argument", {
  expect_error(logger_factory("X", debug = "yes"),        "single logical")
  expect_error(logger_factory("X", debug = c(TRUE, FALSE)), "single logical")
  expect_error(logger_factory("X", debug = 1L),           "single logical")
})

test_that("info() emits a message with the prefix and tag", {
  log <- logger_factory("Greeter")
  expect_message(log$info("hello"), "Greeter")
  expect_message(log$info("hello"), "hello")
  expect_message(log$info("hello"), "\\[INFO\\]")
})

test_that("warning() emits a warning with the prefix", {
  log <- logger_factory("X")
  expect_warning(log$warning("watch out"), "watch out")
  expect_warning(log$warning("watch out"), "X")
})

test_that("error() throws an error with the prefix", {
  log <- logger_factory("X")
  expect_error(log$error("boom"), "boom")
  expect_error(log$error("boom"), "X")
})

test_that("debug() is silent when logger.debug option is FALSE", {
  log <- logger_factory("Dbg")
  withr::with_options(list(logger.debug = FALSE), {
    expect_silent(log$debug("noise"))
  })
})

test_that("debug() emits a message when logger.debug option is TRUE", {
  log <- logger_factory("Dbg")
  withr::with_options(list(logger.debug = TRUE), {
    expect_message(log$debug("noise"), "noise")
    expect_message(log$debug("noise"), "\\[DEBUG\\]")
  })
})

test_that("debug() instance override TRUE emits regardless of option", {
  log <- logger_factory("Dbg", debug = TRUE)
  withr::with_options(list(logger.debug = FALSE), {
    expect_message(log$debug("forced on"), "forced on")
  })
})

test_that("debug() instance override FALSE suppresses regardless of option", {
  log <- logger_factory("Dbg", debug = FALSE)
  withr::with_options(list(logger.debug = TRUE), {
    expect_silent(log$debug("silenced"))
  })
})

test_that("call. = FALSE default can be overridden by the caller", {
  log <- logger_factory("X")
  err <- tryCatch(log$error("boom", call. = TRUE), error = function(e) e)
  expect_false(is.null(err$call))
})

test_that("format.logger returns a character string describing the logger", {
  log <- logger_factory("MyApp")
  out <- format(log)
  expect_type(out, "character")
  expect_match(out, "MyApp")
  expect_match(out, "logger.debug")
})

test_that("format.logger reflects instance-level debug override", {
  expect_match(format(logger_factory("A", debug = TRUE)),  "TRUE")
  expect_match(format(logger_factory("A", debug = FALSE)), "FALSE")
})

test_that("print.logger outputs text and returns the logger invisibly", {
  log <- logger_factory("P")
  out <- withVisible(print(log))
  expect_false(out$visible)
  expect_s3_class(out$value, "logger")
})

