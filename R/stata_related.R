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

#' Set attributes for variables in data.table
#' 
#' @description Set attributes for variables in data.table
#'
#' @param df data.table
#' @param variable one bare variabel name or character vector of variable names
#' @param attributes numeric or character vector, length must equal variables
#' @export
stlabel <- function(df, variable, attribute, type = "label") {
    nn <- deparse(substitute(variable))
    if (!is.data.frame(df)) 
       stop("df must be a data.table") 
    if (nn %in% names(df)) 
        variable <- nn 
    if (any(! variable %in% names(df)))
        stop("some variable not exist")
    if (length(variable) != length(attribute))
        stop("'variable' number must equal 'attribute' number")

    if (length(type) != 1 || !is.character(type))
        stop("attribute type must be one string")

    for (i in seq_along(variable)) {
        vari <- variable[i]
        attr <- attribute[i]
        setattr(df[[variable]], type, attr)
    }
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
                      big.mark = ",", na.replace = "n.a.") {
    one <- function(z, nsmall, width, digits, na.replace, big.mark) {
        if (is.na(z)) return(na.replace)
        if (is.integer(z)) return(format(z, digits = 0, nsmall = 0,
                                         width = width, big.mark = big.mark))
        t <- abs(z)
        n <- if (t == 0) {
                format(z, digits = 0, nsmall = nsmall, width = width, big.mark = big.mark)
            } else if (t < 1) {
                format(z, nsmall = nsmall, width = width,
                       digits = max(0, digits - as.integer(log10(1/t))),
                       , big.mark = big.mark)
            } else if (t < 10) {
                format(z, nsmall = nsmall, width = width, digits = digits,
                       big.mark = big.mark )
            } else if (log10(t) < digits + 1) {
                format(z, digits = digits - as.integer(log10(t)),
                       nsmall = max(0, nsmall - as.integer(log10(t))),
                       width = width, big.mark = big.mark)
            } else {
                format(z, digits = 0, width = width, big.mark = big.mark)
            }
    }
    if (!is.numeric(x)) stop("x must be numeric vector")
    as.character(lapply(x, one, nsmall, width, digits, na.replace, big.mark))
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
    nn <- deparse(substitute(variable))
    if (!is.data.frame(df)) 
       stop("df must be a data.frame") 
    if (nn %in% names(df)) {
        variable <- nn
    } else if (!grepl("[\"']", nn) & !exists(nn, mode = "character")) {
        stop(nn, " is not a character object") 
    }
    if (any(! variable %in% names(df)))
        stop("some variable not exist") 

    gen_label <- function(name) {
        label <- attr(df[[name]], "label")
        if (is.null(label)) label <- name
        label
    }
    if (!is.null(label) && label != TRUE) {
        if (length(variable) != length(label))
            stop("'label' must be NULL, TREU or a character vector having same length with variables")
    } else if (!is.null(label) && label == TRUE) {
        label <- as.character(lapply(variable, gen_label))
    }

    sum_by_varname <- function(x) {
        stsum.default(df[[x]], na.rm, format, digits, nsmall, width, big.mark, quietly = TRUE)
    }
    m.temp <- sapply(variable, sum_by_varname)
    df.new <- t(m.temp)
    df.new <- as.data.frame(df.new, row.names = "")
    df.new <- if (!is.null(label)) {
        cbind(varlabel = format(label, justify = "right", width = 8), df.new)
    } else {
        cbind(varName = format(variable, justify = "right", width = 8), df.new)
    }
    df.new
}

#' set data.table as panel data
#'
#' @description Mark `data.table` as Panel data, and set `id` and `time`
#' @param data a data.table object
#' @param id variable name as panel id
#' @param time variable name as panel time
#' @export
stxtset <- function(data, id, time) {
    #> 将 `data.table` 数据标记为面板数据
    #> data `data.table` 
    #> id   变量名, 用于指示个体
    #> time 变量名, 用于指示时间
    #> 将 `data.table` 的 `key` 设置为 `id` + `time`
    #> 增加一个属性 `Panel`, 设置成功后, 将该属性标记为 `TRUE`
    #> 增加参数的 robust

    id_name   <- gsub("[\"']", "", deparse(substitute(id)))
    time_name <- gsub("[\"']", "", deparse(substitute(time)))
    if (! id_name %in% names(data))   id_name <- id
    if (! time_name %in% names(data)) time_name <- time

    setattr(data, "xt", c(id_name, time_name))
    check_result <- stxtcheck(data)
    if (!check_result[[1]]) {
        stop("panel set failed:\n", check_result[[2]])
    }
    invisible(data)
}

#' Calculate lagged value
#'
#' @description Calculate n lagged value of x relate to time
#' @param x a vector
#' @param time a interger vector, length must equal to x
#' @param n an integer
#' @export
stlag <- function(x, time, n = 1L) {
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

#' check whether a data.table is a panel table
#'
#' @param dt data.table object
#' @export
stxtcheck <- function(dt) {
    xt <- attr(dt, "xt")
    if (!is.data.table(dt))
        return(list(FASLE, "dt isnot a data.table"))
    if (is.null(xt)) 
        return(list(FALSE, message = "Not set attribute xt yet"))
    if (length(xt) != 2)
        return(list(FALSE, message = "attribute xt can only contain two varname"))
    if (!is.character(xt))
        return(list(FALSE, message = "attribute xt need to be a character vector"))
    if (!all(xt %in% names(dt)))
        return(list(FALSE, message = "attribute xt contain varaible not in data.table"))
    if (!is.integer(dt[[xt[2]]]))
        return(list(FALSE, message = "time variable must point to an integer vector"))
    if (anyDuplicated(dt[, xt]) != 0)
        return(list(FALSE, message = paste("exists duplicates by", xt[1], "and", xt[2])))
    return(list(TRUE, message = paste("The data.table is a panel table")))
}

#' Stimulate panel time operator in stata
#'
#' @description stata-style panel time operator.
#'
#' @param panel.dt an data.table object with a TRUE attribute "panel",
#'        which is setted through `stxtset()`
#' @param varlist variable names needed to calculate lagged value
#' @param n an interger, determine lag direction and interval
#' @param new.df a logic value, determine whether to generate a new data.table.
#'        default is `FALSE`
#' @examples
#' library(data.table)
#' dt <- data.table(
#'     id = rep(c("a", "b", "c", "d"), each = 4),
#'     time = rep(2001:2004, 4),
#'     x = sample(16)
#' )
#' stxtset(dt, id, time)
#' (stxtlag(dt, x))
#' @export
stxtlag <- function(panel.dt, varlist, n = 1L, new.dt = FALSE) {
    if (length(n) != 1)
        stop("n's length must equal to 1")
    if (!is.integer(n) | n == 0)
        stop("n must be a non-zero interge")

    if (is.symbol(substitute(varlist)) & 
        deparse(substitute(varlist)) %in% names(panel.dt)) 
        varlist <- deparse(substitute(varlist))
    if (! all(varlist %in% names(panel.dt)))
        stop("Some value in varlist isnot exist!")

    if (!stxtcheck(panel.dt)[[1]])
        stop("\npanel check error:\n", stxtcheck(panel.dt)[[2]])
    id <- attr(panel.dt, "xt")[1]
    time <- attr(panel.dt, "xt")[2]

    k.lag.varlist <- if (n > 0) {
        paste0("L", n, ".", varlist)
    } else {
        paste0("F", -n, ".", varlist)
    }

    if (new.dt == TRUE) {
        dt <- panel.dt[, lapply(.SD[, -1], stlag, .SD[[1]], n),
                             by = c(id), .SDcols = c(time, varlist)][, -1]
        names(dt) <- k.lag.varlist
    } else {
        print(c(time, varlist))
        panel.dt[, c(k.lag.varlist) := lapply(.SD[, -1], stlag, .SD[[1]], n),
                 by = id, .SDcols = c(time, varlist)]
    }
    invisible(panel.dt)
}

