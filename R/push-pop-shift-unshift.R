# Inspired:
#   mpettis/push-pop-shift-unshift.R
#   https://gist.github.com/mpettis/b7bfeff282e3b052684f
#
# https://gist.github.com/leeper/d6d085ac86d1e006167e
# http://www.johnmyleswhite.com/notebook/2009/12/07/implementing-push-and-pop-in-r/
#

#' perl-style push
#'
#' @export
push <- function(x, ...) {
    values <- list(...)
    stopifnot(length(values) > 0L)
    outer_x <- as.character(substitute(x))

    for (i in seq_along(values)) {
        value <- values[[i]]
        name <- if (is.null(names(values))) NULL else names(values)[i]
        if (is.list(x)) value <- list(value)
        x <- c(x, value)
        if (!is.null(name)) names(x)[length(x)] <- name
    }

    if (!is.null(names(x)))
        names(x) <- ifelse(is.na(names(x)), "", names(x))

    assign(outer_x, x, parent.frame())
    invisible(get(outer_x, parent.frame()))
}

#' perl-style pop
#'
#' @export
pop <- function(x, drop = TRUE) {
    if (length(x) == 0) return(NULL)
    outer_x <- as.character(substitute(x))
    popret <- if (isTRUE(drop) && is.list(x)) {
        x[[length(x)]]
    } else {
        x[length(x), drop = drop]
    }
    assign(outer_x, x[-length(x)], parent.frame())
    popret
}

#' perl-style unshift
#'
#' @export
unshift <- function(x, ...) {
    values <- list(...)
    stopifnot(length(values) > 0L)
    outer_x <- as.character(substitute(x))

    for (i in seq_along(values)) {
        value <- values[[i]]
        name <- if (is.null(names(values))) NULL else names(values)[i]
        if (is.list(x)) value <- list(value)
        x <- c(value, x)
        if (!is.null(name)) names(x)[1] <- name
    }

    if (!is.null(names(x)))
        names(x) <- ifelse(is.na(names(x)), "", names(x))

    assign(outer_x, x, parent.frame())
    invisible(get(outer_x, parent.frame()))
}

#' perl-style shift
#'
#' @export
shift <- function(x, drop = TRUE) {
    if (length(x) == 0) return(NULL)
    outer_x <- as.character(substitute(x))
    shiftret <- if (isTRUE(drop) && is.list(x)) {
        x[[1]]
    } else {
        x[1, drop = drop]
    }
  assign(as.character(substitute(x)), x[-1], parent.frame())
  shiftret
}

