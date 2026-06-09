# cache_result ---------------------------------------------------------------

test_that("cache_result returns the evaluated result", {
  skip_if_not_installed("fs")
  withr::with_tempdir({
    expect_equal(cache_result(1 + 1, "out.rds"), 2)
  })
})

test_that("cache_result places a bare filename in cache/ subdirectory", {
  skip_if_not_installed("fs")
  withr::with_tempdir({
    cache_result(TRUE, "flag.rds")
    expect_true(file.exists(file.path("cache", "flag.rds")))
  })
})

test_that("cache_result returns cached value on subsequent calls", {
  skip_if_not_installed("fs")
  withr::with_tempdir({
    cache_result(1L, "out.rds")
    # Expression differs but file exists — must come from cache
    expect_equal(cache_result(99L, "out.rds"), 1L)
  })
})

test_that("cache_result re-evaluates when update = TRUE", {
  skip_if_not_installed("fs")
  withr::with_tempdir({
    cache_result(1L, "out.rds")
    expect_equal(cache_result(99L, "out.rds", update = TRUE), 99L)
  })
})

test_that("cache_result respects an explicit directory in the path", {
  skip_if_not_installed("fs")
  withr::with_tempdir({
    dir.create("mydir")
    cache_result(42L, "mydir/out.rds")
    expect_true(file.exists("mydir/out.rds"))
    expect_false(file.exists(file.path("cache", "out.rds")))
  })
})

# cache_object ----------------------------------------------------------------

test_that("cache_object stores the result in the provided environment", {
  env <- new.env(parent = emptyenv())
  cache_object("x", 42L, cache = env)
  expect_true(exists("x", envir = env, inherits = FALSE))
  expect_equal(get("x", envir = env), 42L)
})

test_that("cache_object does not re-evaluate when key already exists", {
  env <- new.env(parent = emptyenv())
  assign("x", 10L, envir = env)
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  cache_object("x", { calls$n <- calls$n + 1L; 99L }, cache = env)
  expect_equal(calls$n, 0L)    # expression was not evaluated
  expect_equal(get("x", envir = env), 10L)
})

test_that("cache_object re-evaluates when update = TRUE", {
  env <- new.env(parent = emptyenv())
  assign("x", 1L, envir = env)
  cache_object("x", 2L, cache = env, update = TRUE)
  expect_equal(get("x", envir = env), 2L)
})

test_that("cache_object rejects non-environment cache argument", {
  expect_error(cache_object("x", 1L, cache = list()), "is.environment")
})

# add_cache -------------------------------------------------------------------

test_that("add_cache rejects a non-function", {
  expect_error(add_cache(42), "f must be a function")
})

test_that("add_cache rejects invalid expire_days", {
  expect_error(add_cache(identity, expire_days = -1),      "expire_days must be positive")
  expect_error(add_cache(identity, expire_days = "7"),     "expire_days must be numeric")
  expect_error(add_cache(identity, expire_days = c(1, 2)), "single value")
})

test_that("add_cache returns a function preserving original formals", {
  skip_if_not_installed("cachem")
  skip_if_not_installed("memoise")
  skip_if_not_installed("qs")
  f <- function(x, n = 10L) x
  withr::with_tempdir({
    withr::with_envvar(c(XDG_CACHE_HOME = getwd()), {
      cached_f <- add_cache(f, subdir = "test_formals")
      expect_type(cached_f, "closure")
      nms <- names(formals(cached_f))
      expect_true("x"      %in% nms)
      expect_true("n"      %in% nms)
      expect_true("update" %in% nms)
    })
  })
})

test_that("add_cache uses '.update' when original function already has 'update'", {
  skip_if_not_installed("cachem")
  skip_if_not_installed("memoise")
  skip_if_not_installed("qs")
  f <- function(x, update = FALSE) x
  withr::with_tempdir({
    withr::with_envvar(c(XDG_CACHE_HOME = getwd()), {
      cached_f <- add_cache(f, subdir = "test_rename")
      expect_true(".update" %in% names(formals(cached_f)))
    })
  })
})
