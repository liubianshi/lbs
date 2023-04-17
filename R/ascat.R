#' Convert number to factor by inner breaks
#'
#' @param x a numeric vector
#' @param breaks a numeric vector of two or more unique cut points
#' @param labels labels for the levels of the resulting category
#' @return A `factor` is returned
#' @examples
#' ascat(1:10, 5) # eqaul cut(1:10, c(1, 5, 10), include.lowest = TRUE)
#' ascat(1:10, 5, c("less than or euqal to 5", "greater than 5"))
#' @export
ascat <- function(x, breaks, labels = NULL, include.lowest = TRUE) {
    stopifnot(all(sort(breaks) == breaks))
    stopifnot(length(unique(breaks)) == length(breaks))
    stopifnot(!any(is.na(breaks)))

    min <- min(x, na.rm = TRUE)
    max <- max(x, na.rm = TRUE)
    if (breaks[1] > min)
        breaks <- append(breaks, min, 0)
    if (breaks[length(breaks)] < max)
        breaks <- append(breaks, max)

    if (is.null(labels)) {
        l <- length(breaks)
        labels        <- gettextf("(%d,%d]", breaks[-l], breaks[-1])
        labels[1]     <- gettextf("[%d,%d]", min, breaks[2])
        labels[l - 1] <- gettextf("(%d,%d]", breaks[l-1], max)
        rm(l)
    }
    if (isFALSE(include.lowest)) {
        labels[1] <- gettextf("(%d,%d]", min, breaks[2])
        breaks <- append(breaks, -Inf, 0)
        labels <- append(labels, gettextf("[%d]", min), 0)
    }
    stopifnot(length(breaks) - 1 == length(labels))
    cut(x, breaks, labels, include.lowest = include.lowest)
}
