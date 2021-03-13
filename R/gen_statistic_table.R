#' Generate Statistic Result table
#' 
#' @description Gen Academic table
#' @param stat statistic data.frame
#' @param p p Value data.frame
#' @param se standard error data.frame
#' @param t t statistic  data.frame
#' @param star a list contain cut and symbol
#' @param foramt a list contain format information
#'
#' @export
gen_statistic_table <- function(
    stat, p, se = NULL, t = NULL,
    format = c(stat = "3", se = "[3]"),
    star = list(cut = c(0.1, 0.05, 0.01), symbol = c("*", "**", "***")),
    outfmt = "md"
) {
    if (!hasName(format, "stat")) format <- c(stat = "3", format)
    stopifnot(names(format)[1] == "stat")
    out <- vector("list", length(format))
    names(out) <- names(format)

    out$stat <- coef2str(stat, parse_c(format["stat"]))
    if (!is.null(star)) {
        star_table <- purrr::map_dfc(p, genstar, adjstar(star, outfmt))
        out$stat <- purrr::map2_dfc(out$stat, star_table, paste0)
    }
    out$stat$`__1` <- 1
    out$stat$`__2` <- seq_len(nrow(out$stat))

    for (i in seq_along(format)[-1]) {
        out[[i]] <- coef2str(get(names(format)[i]), parse_c(format[i]))
        out[[i]]$`__1` <- i
        out[[i]]$`__2` <- seq_len(nrow(out[[i]]))
    }
    out

    data.table::rbindlist(out) %>%
        data.table::setorderv(c("__2", "__1")) %>%
        .[, !c("__1", "__2")]
}

# strformat: format numeric vector --------------------------------------------
strformat <- function(x, digits = 3L, nsmall = 0L, width = NULL,
                      big.mark = ",", na.replace = "") {
    stopifnot(is.numeric(x))
    if (is.integer(x)) return(as.character(x))
    one <- function(z, nsmall, width, digits, na.replace, big.mark) {
        stopifnot(is.numeric(z) && length(z) == 1L)
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
    as.character(lapply(x, one, nsmall, width, digits, na.replace, big.mark))
}

parse_c <- function(char) {
    if (is.null(char) || is.na(char) || length(char) == 0L)
        return(NULL)
    stopifnot(length(char) == 1L)
    stopifnot(nchar(char) %in% c(1, 3))
    fmt <- if (nchar(char) == 1L) {
        c("", char, "")
    } else {
        strsplit(char, "")[[1]]
    }
    stopifnot(grepl("^\\d$", fmt[2]))
    stopifnot(!any(grepl("^[A-Za-z0-9]$", fmt[c(1,3)])))
    fmt
}

coef2str <- function(data, fmt) {
    # for example: c("(", "2", ")")
    stopifnot(length(fmt) == 3)
    l_par = fmt[1]
    r_par = fmt[3]
    digits = as.integer(fmt[2])

    regnames <- names(data)[purrr::map_lgl(data, is.numeric)]
    fm <- function(x, digits = NULL, l_par, r_par) {
        y <- strformat(x, digits = digits, nsmall = digits, na.replace = "") %>% trimws()
        ifelse(y == "", "", paste0(l_par, y, r_par))
    }
    for (i in seq_along(regnames))
        data[[regnames[i]]] %<>% fm(digits, l_par, r_par)
    data
}

complete_names <- function(obj, n) {
    nms <- names(obj)
    nms_null <- if (is.null(nms)) seq_along(obj) else which(nms == "")
    nms_not_null <- nms[which(nms != "")]

    n <- n[!(n %in% nms_not_null)]
    nms[nms_null] <- n[seq_along(nms_null)]
    nms[is.na(nms)] <- ""
    nms
}

adjstar <- function(star, outfmt = "text") {
    if (is.null(star)) return(NULL)
    names(star) = complete_names(star, c("cut", "symbol"))
    starcut <- star$cut
    starsymbol <- star$symbol

    stopifnot(is.numeric(starcut))
    stopifnot(max(starcut) < 1 && min(starcut) > 0)
    starcut <- unique(sort(starcut, decreasing = TRUE))

    stopifnot(is.character(starsymbol))
    stopifnot(all(!grepl("[0-9A-Za-z]", starsymbol)))
    stopifnot(length(starsymbol) >= length(starcut))
    starsymbol <- starsymbol[seq_along(starcut)] 

    if (outfmt %in% c("flextable", "docx", "word")) {
        starsymbol <- paste0("^", starsymbol, "^")
    } else if (outfmt %in% c("html", "pdf", "kable", "md")) {
        starsymbol <- escape(starsymbol, chars = "*^_`~")
        starsymbol <- paste0("^", starsymbol, "^")
    }
    list(cut = starcut, symbol = starsymbol)
}

# escape: escape specific chars -----------------------------------------------
escape <- function(x, chars = "*\\") {
    stopifnot(length(chars) == 1)

    exit_esc = grepl("\\\\", chars)
    x <- gsub("\\", "\\\\", x, fixed = TRUE)
    chars = gsub("\\", "", chars, fixed = TRUE)

    char_list = strsplit(chars, "")[[1]]
    for (ch in char_list) {
        x <- gsub(ch, paste0("\\", ch), x, fixed = TRUE)
    }
    x
}

genstar <- function(pvalue, star) {
    starcut <- star$cut
    starsymbol <- star$symbol

    if (!is.numeric(pvalue)) return(pvalue)
    stopifnot(max(pvalue, na.rm = TRUE) < 1)
    stopifnot(min(pvalue, na.rm = TRUE) >= 0)

    star <- ifelse(is.na(pvalue), NA, "") 
    for (i in seq_along(starcut)) {
        star <- ifelse(pvalue <= starcut[i], starsymbol[i], star)
        star %<>% rempty("")
    }
    star
}

# rempty: replace empty with specific value -----------------------------------
rempty <- function(x, r, empty = NULL) {
    stopifnot(length(r) == 1L || length(r) == length(x))
    stopifnot(typeof(x) == typeof(r))
    x <- ifelse(is.na(x) | x %in% empty, r, x)
    x
}
