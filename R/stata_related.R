#' des variables
#'
#' @description Describe the basic information of variables in stata-style
#' 
#' @param df `data.frame`
#' @examples
#' df <- data.frame(a = 1:3, b = 2:4)
#' stdes(df)
#'
#' attr(df$a, "label") = "A"
#' attr(df$b, "label") = "B"
#' stdes(df)
#' @export
stdes <- function(df) {  #> 载入自定义函数
    get_type_label <- function(x) {
        if ("label" %in% names(attributes(x))) 
            c(typeof(x), attr(x, "label"))
        else c(typeof(x), "")
    }
    label <- sapply(df, get_type_label)
    label <- t(label)
    label <- as.data.frame(label)
    colnames(label) <- c("type", "label")
    label$variable <- rownames(label)
    rownames(label) = NULL
    subset(label, select = c("variable", "type", "label"))
}

#' Set attributes for variables in data.frame
#'
#' @description Set attributes for variables in data.table
#'
#' @param df data.table
#' @param var one bare variable name or character vector of variable names
#' @param attr numeric or character vector, length must equal `var`
#' @param type name of the attribute to set (default `"label"`)
#' @export
stlabel <- function(df, var, attr, type = "label") {
    var <- rlang::enquo(var)
    varlist <- get_df_names(df, !!var)
    stopifnot(length(varlist) == length(attr))
    purrr::walk2(varlist, attr, ~ data.table::setattr(df[[.x]], name = type, .y))
}

#' format number in a reasonable way
#'
#' @description format number in a reasonable way.
#' @param x numeric vector
#' @param digits how many significant digits are to be used for
#'        numeric and complex `x`.  The default, `NULL`, uses
#'        `getOption("digits")`.
#' @param nsmall the minimum number of digits to the right of the
#'        decimal point in formatting real/complex numbers in
#'        non-scientific formats.
#' @param width the _minimum_ field width or `NULL`/`0` for no restriction.
#' @param big.mark character used as thousands separator in formatted output.
#' @param na.replace string used to replace `NA` values in the output.
#' @examples
#' stformat(10 ^ seq(-10, 10) + runif(21))
#' stformat(10 ^ seq(-10, 10) + runif(21), nsmall = 2)
#' stformat(10 ^ seq(-10, 10) + runif(21), digits = 4)
#' stformat(10 ^ seq(-10, 10) + runif(21), width = 7)
#' stformat(10 ^ seq(-10, 10) + runif(21), width = 11)
#' stformat(c(NA, 10 ^ seq(-5, 5)) + runif(12))
#' @export
stformat <- function(x, digits = 3L, nsmall = 3L, width = NULL,
                      big.mark = ",", na.replace = "") {
    if (!is.numeric(x)) 
        stop("x must be numeric vector")
    if (is.integer(x))
        return(format(x, width = width, big.mark = big.mark))
    as.character(lapply(x, format_one_num, nsmall, width, digits, na.replace, big.mark))
}
format_one_num <- function(z, nsmall, width, digits, na.replace, big.mark) {
    if (is.na(z)) return(na.replace)
    stopifnot(is.numeric(z) && length(z) == 1L)
    if (is.null(digits)) stop("Must set digits", call. = FALSE)
    digits <- as.integer(digits)
    if (is.integer(z)) {
        return(format(z, width = width, big.mark = big.mark))
    }
    if (is.null(width)) {
        width = digits + 3
    } else if (width <= digits) {
        stop("Error: width must larger than digits", call. = FALSE)
    }
    fo <- function(x, d = NULL, n = 0L, w = NULL, b = "") {
        format(x, digits = d, nsmall = n, width = w, big.mark = b)
    }
    decbits <- if (abs(z) < 1) {
        width - 0
    } else {
        width - as.integer(log10(abs(round(z, digits = 0L)))) - 2
    }
    if (decbits >= digits) {
       fo(round(z, digits = digits),  digits, digits, width)
    } else if (decbits > 0) {
       fo(round(z, digits = decbits), decbits, decbits, width, big.mark)
    } else {
       fo(round(z, digits = 0), w = width, b = big.mark)
    }
}


#' stata-style sumarisze
#'
#' @description generate a stata-style sumarisze table
#' @param object numerical vector or data.frame. If the object is a data.frame,
#'   `variable` is required.
#' @param ... arguments forwarded to the method (see `stsum.default` and
#'   `stsum.data.frame`).
#' @export
stsum <- function(object, ...) {
    UseMethod("stsum")
}

#' @describeIn stsum default method for numeric vectors.
#' @param na.rm logical; if TRUE drop NAs before summarising.
#' @param format logical; if TRUE format numeric output via `stformat()`.
#' @param digits passed to `stformat()`.
#' @param nsmall passed to `stformat()`.
#' @param width passed to `stformat()`.
#' @param big.mark passed to `stformat()`.
#' @param quietly logical; if TRUE suppress the printed table.
#' @export
stsum.default <- function(object, na.rm = TRUE, format = TRUE,
                           digits = getOption("digits"),
                           nsmall = 3L, width = 7L, big.mark = ",",
                           quietly = FALSE, ...) {
    if (na.rm == TRUE) object <- object[!is.na(object)]
    y <- c(length(object), mean(object), stats::sd(object), min(object),
           stats::quantile(object, 0.250), stats::quantile(object, 0.500),
           stats::quantile(object, 0.750), max(object))
    z <- if (format == TRUE) {
            stformat(y, digits = digits, nsmall = nsmall,
                     width = width, big.mark = big.mark)
    } else {
        as.character(y)
    }
    names(z) <- c("obs", "mean", "sd", "min", "p25", "p50", "p75", "max")
    if (!quietly) print(z, quote = FALSE)
    invisible(z)
}

#' @describeIn stsum data.frame method summarising one or more columns.
#' @param variable bare name or character vector of variable names in `object`.
#' @param label `NULL`, `TRUE`, or a character vector of variable labels;
#'   length must equal `variable` unless `NULL` or `TRUE`.
#' @export
stsum.data.frame <- function(object, variable, label = NULL, na.rm = TRUE,
                              format = TRUE, digits = getOption("digits"),
                              nsmall = 3, width = 7, big.mark = ",", ...) {
    vari <- rlang::enquo(variable)
    variable <- get_df_names(object, !!vari)
    if (isTRUE(label)) {
        label <- as.character(
            lapply(
                variable,
                function(name) {
                    label <- attr(object[[name]], "label")
                    if (is.null(label)) label <- name
                    label
                }
            )
        )
    } else if (!is.null(label) && length(variable) != length(label)) {
        stop("The number of labels are not equal the number of variables")
    }

    sum_by_varname <- function(x) {
        stsum.default(object[[x]], na.rm, format, digits, nsmall, width, big.mark, quietly = TRUE)
    }
    m.temp <- sapply(variable, sum_by_varname)
    df.new <- as.data.frame(t(m.temp), row.names = "")
    df.new <- if (!is.null(label)) {
        cbind(varlabel = format(label, justify = "right", width = 8), df.new)
    } else {
        cbind(varName = format(variable, justify = "right", width = 8), df.new)
    }
    df.new
}

#' Trans numeric vector to character with specific format
#'
#' @param x a numeric vector
#' @param fmt a character vector of format strings
#' @export
stenstring <- function(x, fmt = "%d") {
    stopifnot(is.numeric(x))
    ifelse(isempty(x), NA_character_, gettextf(fmt, x))
}
