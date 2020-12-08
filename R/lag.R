#' Lag/Forward operator in time series
#'
#' @description Calculate the lag/forward terms of variables and vectors
#'
#' @param x a vector.
#' @param df a a data.table or data.frame object.
#'        If hoping update by reference, data.table is required.
#' @param varlist Variable names needed to calculate lagged terms.
#'        a bare name and a character vector are acceptable.
#' @param by Names of variables used to grouping data.
#' @param time a interger vector object for x, and a name or string for df. 
#'        For x, time length must equal to x.
#' @param n an integer.
#' @param mode `NULL` or character string nameing an atomic mode or "list".
#'        Default is `NULL`, update data.frame by reference using `:=` from 
#'        `data.table`. `list`, `data.table`, `data.frame` and `vector` is
#'        acceptable. If `mode = "vector"`, the length of `varlist` needed
#'        to be one.
#'
#' @examples
#' time = rep(2001:2006)
#' x = sample(6)
#' stlag(x, time)
#'
#' ts <- data.table(time = 2001:2009, x = 1:9, y = sample(letters, 9))
#' stlag(ts, x, time = "time", mode = "list")
#' stlag(ts, x, time)
#' stlag(ts, y, time, n = -1L)
#' xt <- data.frame(
#'     id = rep(c("a", "b"), each = 5),
#'     time = rep(2001:2005, 2),
#'     x = 1:10,
#'     y = sample(letters, 10, replace = TRUE),
#'     z = sample(LETTERS, 10, replace = TRUE)
#' )
#' stxtset(xt, id, time)
#' stlag(xt, x, n = -1L, mode = "list")
#' stlag(setDT(xt), x)
#' xt
#' @export
stlag <- function(x, ...) {
    UseMethod("stlag")
}

#' @describeIn stlag Calculate lag term of vector
#'
#' @export
stlag.default <- function(x, time, n = 1L) {
    if (is.list(x)) {
        df <- as.data.table(x)
        stlag.data.frame(x, time, n)
    }
    if (length(n) != 1)
        stop("n's length must equal to 1")
    if (! is.integer(n) || n == 0)
        stop("n must be a non-zero interger")
    if (length(x) != length(time))
        stop("length of x and time need equal")
    if (anyDuplicated(time, incomparables = NA) != 0)
        stop("duplicates in \"time\"")
    index <- match(time - n, time, incomparables = NA)
    x[index]
}

#' @describeIn stlag Calculate lag terms of data.frame/data.table variables
#' 
#' @export
stlag.data.frame <- function(df, varlist = NULL, time = NULL, by = NULL,
                             n = 1L, mode = NULL) {
    names_df <- names(df)

    # 将 data.frame 转化为 data.table
    if (isFALSE(is.data.table(df))) {
        if (is.null(mode)) stop("Update by reference needing a data.table")
        setDT(df)
    }

    # 变量名裸字转换为变量名向量
    time     <- rlang::enquo(time)
    time     <- get_df_names(df, !!time)
    varlist  <- rlang::enquo(varlist)
    varlist  <- get_df_names(df, !!varlist)
    by       <- rlang::enquo(by)
    by       <- get_df_names(df, !!by)

    # 时间变量的验证
    if (isempty(time)) {
        if (isempty(by)) {
            if (stxtcheck(df)[[1]]) {
                xt   <- attr(df, "xt")
                by   <- xt[1]
                time <- xt[2]
            } else if (sttscheck(df)[[1]]) {
                time <- attr(df, "ts")
            } else {
                stop("time variable isnot setting")
            }
        } else {
            stop("Cannot set \"by\" without setting \"time\"")
        }
    }
    if (isempty(time))           stop("Time variable setting error")
    if (length(time) != 1)       stop("only one time variable is allowed")
    if (!is.integer(df[[time]])) stop("time variable must point to an integer vector")

    # 待求 Lag 的变量
    if (all(isempty(varlist))) varlist <- names_df
    varlist <- setdiff(varlist, c(time, by))
    k.lag.varlist <- gen_lag_name(varlist, n)
    if (length(varlist) != 1L && isTRUE(mode == "vector"))
        stop("Mode vector is available when only one variable")

    # 在 data.table 上进行修改
    if (isempty(mode)) {
        if (all(isempty(by))) {
            df[, (k.lag.varlist) := lapply(.SD[, -1], stlag.default,
                                           .SD[[1]], ..n),
                .SDcols = c(time, varlist)]
        } else {
            df[, (k.lag.varlist) := lapply(.SD[, -1], stlag.default,
                                           .SD[[1]], ..n),
                by = c(by), .SDcols = c(time, varlist)]
        }
        return(df)
    }

    # 输出制定模式的结果
    new.df <- if (all(isempty(by))) {
        df[, c(.SD[, 1], lapply(.SD[, -1], stlag.default, .SD[[1]], ..n)),
             .SDcols = c(time, varlist)]
    } else {
        df[, c(.SD[, 1], lapply(.SD[, -1], stlag.default, .SD[[1]], ..n)),
             by = c(by), .SDcols = c(time, varlist)]
    }
    setnames(new.df, c(by, time, k.lag.varlist))
    out <- if (mode == "list") {
        as.list(new.df[, c(..k.lag.varlist)])
    } else if (mode == "data.frame") {
        setDF(new.df)
    } else if (mode == "data.table") {
        new.df
    } else if (mode == "vector") {
        new.df[[k.lag.varlist]]
    }else {
        stop("invalid mode")
    }
    out
}

gen_lag_name <- function(names, n) {
    if (!isTRUE(is.integer(n))) stop("n must be an interger")
    lag_name <- if (n < -1L) {
        paste0("F", -n, ".", names)
    } else if ( n == -1L ) {
        paste0("F.", names)
    } else if ( n == 0L ) {
        names
    }else if ( n == 1L ) {
        paste0("L.", names)
    } else {
        paste0("L", n, ".", names)
    }
    lag_name
}


