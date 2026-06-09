delayedAssign("LOG", logger_factory("Cache"))

# Detect the best available .qs backend (qs2 preferred, qs as fallback).
# Result is cached for the session — requireNamespace() only runs once.
.qs_env <- new.env(parent = emptyenv())
.qs_env$cache <- NULL
.qs_fns <- function() {
  if (!is.null(.qs_env$cache)) return(.qs_env$cache)
  fns <- if (requireNamespace("qs2", quietly = TRUE)) {
    list(read = qs2::qs_read, write = qs2::qs_save)
  } else if (requireNamespace("qs", quietly = TRUE)) {
    list(
      read  = qs::qread,
      write = function(obj, path) qs::qsave(obj, path, preset = "fast")
    )
  } else {
    stop("Package 'qs2' or 'qs' needed for .qs caching. Please install one.", call. = FALSE)
  }
  .qs_env$cache <- fns
  fns
}

#' Cache the value of an expression on disk
#'
#' Evaluate an expression once and persist its result to a cache file.
#' Subsequent calls return the cached value unless `update = TRUE`.
#'
#' File format is inferred from `filename`:
#' - `.rds` uses [base::readRDS()] / [base::saveRDS()]
#' - any other extension is treated as a `qs` cache and uses `qs::qread()` /
#'   `qs::qsave()`
#'
#' @param expressions Expression to cache. Use `{}` to group multiple statements.
#'   The expression is evaluated in a child environment of the caller.
#' @param filename Cache file name or path. If `filename` has no directory
#'   component, or points to the current working directory, the cache is stored
#'   under a local `cache/` subdirectory.
#' @param update Logical scalar. If `TRUE`, always recompute the value and
#'   overwrite any existing cache file.
#'
#' @return The evaluated result.
#'
#' @examples
#' \dontrun{
#' # Cache random numbers to cache/my_random_numbers.qs
#' rand_nums <- cache_result(
#'   {
#'     message("Generating random numbers...")
#'     rnorm(10)
#'   },
#'   "my_random_numbers.qs"
#' )
#'
#' # Reuse the cached value on later calls
#' rand_nums_again <- cache_result(rnorm(10), "my_random_numbers.qs")
#'
#' # Force recomputation
#' rand_nums_new <- cache_result(rnorm(10), "my_random_numbers.qs", update = TRUE)
#'
#' # Use RDS instead of qs
#' data_rds <- cache_result(data.frame(x = 1:5, y = letters[1:5]), "my_data.rds")
#' }
#' @export
cache_result <- function(expressions, filename, update = FALSE) {
  if (grepl("\\.rds$", filename, ignore.case = TRUE)) {
    readfile <- readRDS
    writefile <- saveRDS
  } else {
    qfns <- .qs_fns()
    readfile <- qfns$read
    writefile <- qfns$write
  }

  path <- if (fs::path_dir(filename) == ".") {
    fs::path("cache", fs::path_file(filename))
  } else {
    filename
  }
  if (fs::is_link(path)) path <- fs::link_path(path)

  if (fs::file_exists(path) && !isTRUE(update)) {
    return(readfile(path))
  }

  fs::dir_create(fs::path_dir(path), recurse = TRUE)

  expressions_sub <- substitute(expressions)
  eval_env <- new.env(parent = parent.frame(n = 1))
  res <- eval(expressions_sub, envir = eval_env)

  tryCatch(
    writefile(res, path),
    error = function(e) {
      LOG$warning("Failed to write cache file '", path, "': ", e$message)
    }
  )

  return(res)
}


#' Cache an object in an environment or cachem backend
#'
#' Evaluate an expression and store its result under `name`. If a cached value is
#' already available and `update = FALSE`, that value is returned instead.
#'
#' `cache` can be either:
#' - an [environment()], where values are stored with [base::assign()]
#' - a `cachem` cache object, where values are stored with `$set()` / `$get()`
#'
#' @param name Character scalar. Cache key or object name.
#' @param expressions Expression to evaluate when the value is not already cached.
#'   Use `{}` to group multiple statements.
#' @param cache Storage backend. Defaults to the caller environment.
#' @param update Logical scalar. If `TRUE`, recompute and overwrite any cached
#'   value.
#'
#' @return The cached or newly computed value, returned invisibly.
#'
#' @examples
#' env <- new.env(parent = emptyenv())
#'
#' value <- cache_object(
#'   "x",
#'   {
#'     message("computing")
#'     1 + 1
#'   },
#'   cache = env
#' )
#'
#' value <- cache_object("x", stop("will not run"), cache = env)
#'
#' value <- cache_object("x", 3, cache = env, update = TRUE)
#' @export
cache_object <- function(name, expressions, cache = parent.frame(), update = FALSE) {
  caller_env <- parent.frame()

  backend <- if (is.environment(cache) && is.function(cache[["get"]]) && is.function(cache[["set"]])) {
    list(
      has = function(k)    !cachem::is.key_missing(cache$get(k)),
      get = function(k)    cache$get(k),
      set = function(k, v) cache$set(k, v)
    )
  } else {
    stopifnot(is.environment(cache))
    list(
      has = function(k)    utils::hasName(cache, k),
      get = function(k)    get(k, envir = cache),
      set = function(k, v) assign(k, v, envir = cache)
    )
  }

  if (backend$has(name) && isFALSE(update)) return(invisible(backend$get(name)))

  eval_env <- new.env(parent = caller_env)
  res <- eval(substitute(expressions), envir = eval_env)
  backend$set(name, res)
  invisible(res)
}

#' Wrap a Function with Disk-Backed Caching and Expiration
#'
#' Create a cached variant of `f` that stores results on disk, reuses previous
#' results for identical calls, and can refresh stale cache entries after a
#' configurable number of days.
#'
#' Cache files are stored below the XDG cache directory:
#' - `file.path(Sys.getenv("XDG_CACHE_HOME"), "R", "function", subdir)` when
#'   `XDG_CACHE_HOME` is set
#' - otherwise `file.path(Sys.getenv("HOME"), ".cache", "R", "function", subdir)`
#'
#' @param f Function to wrap.
#' @param subdir Optional cache subdirectory name. If `NULL`, a directory name is
#'   inferred from the function environment.
#' @param expire_days Positive numeric scalar. Cache entries older than this many
#'   days are considered expired.
#'
#' @return A function with the same arguments as `f`, plus a cache-control
#'   argument. By default that argument is named `update`; if `f` already has an
#'   argument named `update`, the added argument is renamed to `.update`.
#'
#' @details
#' In interactive sessions, expired cache entries trigger a prompt asking whether
#' to refresh the cached value. In non-interactive sessions, stale cache is used
#' unless the cache-control argument is explicitly set to `TRUE`.
#'
#' Results are stored together with a timestamp so expiration can be checked on
#' later calls.
#'
#' @examples
#' \dontrun{
#' slow_fetch <- function(id) {
#'   Sys.sleep(2)
#'   data.frame(id = id, value = rnorm(10))
#' }
#'
#' fast_fetch <- add_cache(slow_fetch, subdir = "myapp", expire_days = 14)
#'
#' # First call computes and caches
#' res1 <- fast_fetch(123)
#'
#' # Second call reuses the cache
#' res2 <- fast_fetch(123)
#'
#' # Force refresh
#' res3 <- fast_fetch(123, update = TRUE)
#' }
#' @export
cache_fn <- function(f, subdir = NULL, expire_days = 7L) {
  # Input validation with informative error messages
  stopifnot(
    "f must be a function" = is.function(f),
    "expire_days must be numeric" = is.numeric(expire_days),
    "expire_days must be a single value" = length(expire_days) == 1L,
    "expire_days must be positive" = expire_days > 0
  )

  # Force evaluation of f to ensure proper closure capture
  force(f)

  # --- 0. Safety check: Prevent parameter name conflicts ---
  f_args <- formals(f)
  cache_arg_name <- "update"

  if (!is.null(f_args) && "update" %in% names(f_args)) {
    cache_arg_name <- ".update"
    LOG$info(
      "\n  The original function already includes the 'update' parameter,\n",
      sprintf("  the cache control parameter has been automatically renamed to '%s'.", cache_arg_name)
    )
  }

  # Determine cache subdirectory
  if (is.null(subdir)) {
    if (is.primitive(f) || is.null(environment(f))) {
      subdir <- "base"
    } else {
      top_env <- topenv(environment(f))
      env_name <- environmentName(top_env)
      subdir <- if (env_name == "R_GlobalEnv") "user_custom" else sub("^package:", "", env_name)
    }
    if (subdir == "user_custom") {
      LOG$info("No package name detected, caching will be stored in the 'user_custom' directory.")
    }
  }

  # Determine cache path following XDG standard
  base_dir <- Sys.getenv("XDG_CACHE_HOME", unset = file.path(Sys.getenv("HOME"), ".cache"))
  cache_dir <- file.path(base_dir, "R", "function", subdir)
  fs::dir_create(cache_dir, recurse = TRUE)

  # Initialize backend
  # fmt: skip
  qfns <- .qs_fns()
  qs_backend <- cachem::cache_disk(
    dir       = cache_dir,
    extension = ".qs",
    read_fn   = function(path) tryCatch(qfns$read(path), error = \(e) LOG$error("Cache corrupted")),
    write_fn  = qfns$write
  )

  # Internal runner: executes function and packages result with timestamp
  internal_runner <- function(...) list(result = f(...), timestamp = Sys.time())

  # Memoize the internal runner
  memoized_runner <- memoise::memoise(internal_runner, cache = qs_backend)

  wrapper_body <- function(...) {
    user_args <- list(...)

    do_update <- FALSE
    if (cache_arg_name %in% names(user_args)) {
      do_update <- isTRUE(user_args[[cache_arg_name]])
      user_args[[cache_arg_name]] <- NULL
    }

    force_refresh <- function() {
      do.call(memoise::drop_cache(memoized_runner), user_args)
      do.call(memoized_runner, user_args)
    }

    if (do_update) {
      LOG$info("[Forced Update] Re-fetching data...")
      cached_obj <- force_refresh()
    } else {
      cached_obj <- do.call(memoized_runner, user_args)
    }

    time_diff <- as.numeric(difftime(Sys.time(), cached_obj$timestamp, units = "days"))

    if (time_diff > expire_days) {
      cache_time_str <- format(cached_obj$timestamp, "%Y-%m-%d %H:%M")
      LOG$info(sprintf("Cache has expired! Last update: %s (over %g days ago).", cache_time_str, expire_days))

      should_update <- if (interactive()) {
        utils::menu(
          c("Yes (update immediately)", "No (use old data)"),
          title = "Expired cache found, update?"
        ) == 1L
      } else {
        LOG$info("(Non-interactive mode: defaults to using old data. Set `update = TRUE` to update)")
        FALSE
      }

      if (should_update) {
        LOG$info("Updating cache...")
        cached_obj <- force_refresh()
      } else {
        LOG$info("Keeping old cache.")
      }
    }

    cached_obj$result
  }

  # Preserve original function arguments and add update parameter
  extra_arg <- list(FALSE)
  names(extra_arg) <- cache_arg_name

  final_args <- if (is.null(f_args)) {
    LOG$warning("The target function is a Primitive function, unable to retain parameter hints, will use (...)")
    c(alist(... = ), extra_arg)
  } else {
    c(f_args, extra_arg)
  }

  wrapper_env <- new.env(parent = baseenv())
  wrapper_env$memoized_runner <- memoized_runner
  wrapper_env$expire_days     <- expire_days
  wrapper_env$cache_arg_name  <- cache_arg_name

  as.function(c(final_args, body(wrapper_body)), envir = wrapper_env)
}
