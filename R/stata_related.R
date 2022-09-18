#' des variables
#'
#' @description Describe the basic information of variables in stata-style
#' 
#' @param df `data.frame`
#' @examples
#' df <- data.frame(a = 1:3, b = 2:4)
#' des(df)
#'
#' attr(df$a, "label") = "A"
#' attr(df$b, "label") = "B"
#' des(df)
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
#' @param variable one bare variabel name or character vector of variable names
#' @param attributes numeric or character vector, length must equal variables
#' @export
stlabel <- function(df, var, attr, type = "label") {
    var <- rlang::enquo(var)
    varlist <- get_df_names(df, !!var)
    stopifnot(length(varlist) == length(attr))
    purrr::walk2(varlist, attr, ~ setattr(df[[.x]], name = type, .y))
}

#' format number in a reasonable way
#'
#' @description format number in a reasonable way. 
#' @param x, numeric vector
#' @param digits, how many significant digits are to be used for
#'        numeric and complex `x`.  The default, `NULL`, uses
#'        `getOption("digits")`.
#' @param nsmall, the minimum number of digits to the right of the
#'        decimal point in formatting real/complex numbers in
#'        non-scientific formats.
#' @param width: `default` method: the _minimum_ field width or
#'        `NULL` or `0` for no restriction.
#' @param na.replace, used to replace `NA`
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
#' @param object numerical vector of data.frame. If the object is a data.frame,
#' `variable` is required.
#' @param viaralbe variable name of an character vector consists of variable names
#' @param label `NULL`, `TRUE` or an character vector consists of variable labels.
#'      The length of labels must be equal to the length of variables unless the
#'      label is `NULL` or `TREU`
#' @export
stsum<- function(object, ...) {
    UseMethod("stsum")
}

#' @export
stsum.default <- function(x, na.rm = TRUE, format = TRUE,
                           digits = getOption("digits"),
                           nsmall = 3L, width = 7L, big.mark = ",",
                           quietly = FALSE) {
    if (na.rm == TRUE) x <- x[!is.na(x)]
    y <- c(length(x), mean(x), sd(x), min(x), quantile(x, 0.250),
           quantile(x, 0.500), quantile(x, 0.750), max(x))
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

#' @export
stsum.data.frame <- function(df, variable, label = NULL, na.rm = TRUE, 
                              format = TRUE, digits = getOption("digits"),
                              nsmall = 3, width = 7, big.mark = ",") {
    vari <- rlang::enquo(variable)
    variable <- get_df_names(df, !!vari)
    if (isTRUE(label)) {
        label <- as.character(
            lapply(
                variable,
                function(name) {
                    label <- attr(df[[name]], "label")
                    if (is.null(label)) label <- name
                    label
                }
            )
        )
    } else if (!is.null(label) && length(variable) != length(label)) {
        stop("The number of labels are not equal the number of variables")
    }

    sum_by_varname <- function(x) {
        stsum.default(df[[x]], na.rm, format, digits, nsmall, width, big.mark, quietly = TRUE)
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
