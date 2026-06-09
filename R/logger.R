#' Create a Logger Factory
#'
#' Returns an object with four logging methods (`info`, `warning`, `error`,
#' `debug`) that prepend a prefix and ANSI colour codes to each message.
#'
#' @param prefix A single character string prepended (underlined) to every
#'   log message.
#' @param debug `TRUE`/`FALSE` to force debug output on or off for this
#'   logger instance, overriding the global `logger.debug` option.  When
#'   `NULL` (default) the instance defers to `getOption("logger.debug",
#'   FALSE)` at call time, so toggling the option mid-session takes effect
#'   immediately.
#'
#' @return An S3 object of class `logger` — a list of four closures:
#'   \itemize{
#'     \item `info()`  — informational `message()` in blue.
#'     \item `warning()`  — `warning()` in yellow. Passes `call. = FALSE`
#'       unless the caller overrides it.
#'     \item `error()` — `stop()` in red. Passes `call. = FALSE` unless
#'       the caller overrides it.
#'     \item `debug()` — grey `message()`, emitted only when debug is
#'       enabled (see `debug` parameter and `logger.debug` option).
#'   }
#'
#' @examples
#' logger <- logger_factory("MyApp")
#' logger$info("Application started")
#' try(logger$warning("Low memory"))
#'
#' # Enable debug logging via option (affects all loggers with debug = NULL).
#' old <- options(logger.debug = TRUE)
#' logger$debug("Variable x = 5")
#' options(old)
#'
#' # Instance-level override — always debug, regardless of global option.
#' verbose_logger <- logger_factory("MyApp", debug = TRUE)
#' verbose_logger$debug("always shown")
#'
#' # Instance-level override — never debug, regardless of global option.
#' silent_logger <- logger_factory("MyApp", debug = FALSE)
#' silent_logger$debug("never shown")
#'
#' @export
logger_factory <- function(prefix, debug = NULL) {
  if (!is.character(prefix) || length(prefix) != 1L) {
    stop("prefix must be a single character string", call. = FALSE)
  }
  if (!is.null(debug) && (!is.logical(debug) || length(debug) != 1L)) {
    stop("debug must be a single logical value or NULL", call. = FALSE)
  }

  prefix_fmt <- paste0(" \033[4m", prefix, "\033[0m")

  parse_args <- function(...) {
    args <- list(...)
    if (!"call." %in% names(args)) {
      args[["call."]] <- FALSE
    }
    invisible(args)
  }

  is_debug_on <- function() {
    if (!is.null(debug)) debug else isTRUE(getOption("logger.debug", FALSE))
  }

  structure(
    list(
      info = function(...) {
        message("\033[34m[INFO]", prefix_fmt, ": \033[0m", ...)
      },
      warning = function(...) {
        do.call(warning, parse_args("\033[33m[WARN]", prefix_fmt, ": \033[0m", ...))
      },
      error = function(...) {
        do.call(stop, parse_args("\n\033[31m[ERROR]", prefix_fmt, ": \033[0m", ...))
      },
      debug = function(...) {
        if (is_debug_on()) {
          n        <- sys.nframe()
          src      <- attr(sys.call(n), "srcref")
          fn_name  <- if (n > 1L) {
            tryCatch(
              deparse(sys.call(n - 1L)[[1L]], nlines = 1L),
              error = function(e) "?"
            )
          } else {
            "<global>"
          }
          location <- if (!is.null(src)) {
            sf   <- attr(src, "srcfile")
            file <- if (!is.null(sf)) basename(sf$filename) else NULL
            line <- src[1L]
            if (!is.null(file)) paste0(fn_name, "@", file, ":", line)
            else                paste0(fn_name, ":", line)
          } else {
            fn_name
          }
          message("\033[90m[DEBUG][", location, "]", prefix_fmt, ": \033[0m", ...)
        }
      }
    ),
    class  = "logger",
    prefix = prefix,
    debug  = debug
  )
}

#' Format a logger object
#'
#' @param x A `logger` object created by [logger_factory()].
#' @param ... Ignored; present for compatibility with the generic.
#' @return A single character string describing the logger's configuration.
#' @export
format.logger <- function(x, ...) {
  dbg_val  <- attr(x, "debug")
  dbg_desc <- if (is.null(dbg_val)) {
    effective <- isTRUE(getOption("logger.debug", FALSE))
    paste0("NULL  (inherits option 'logger.debug', currently ", effective, ")")
  } else {
    as.character(dbg_val)
  }
  paste0(
    "<logger>\n",
    "  prefix : ", deparse(attr(x, "prefix")), "\n",
    "  debug  : ", dbg_desc
  )
}

#' Print a logger object
#'
#' @param x A `logger` object created by [logger_factory()].
#' @param ... Ignored; present for compatibility with the generic.
#' @return `x`, invisibly.
#' @export
print.logger <- function(x, ...) {
  cat(format(x, ...), "\n")
  invisible(x)
}

