#' 'Not Available' / Missing Values / Empty values / NULL
#'
#' @description In the actual data processing process, the appearance of
#' a null value is either because it is not available, or because the value is
#' invalid, or because it does not exist. Sometimes it is necessary to make
#' a strict distinction, but in many cases, it is only necessary to judge
#' whether a value is Empty value is sufficient.
#' 
#' @param x an R object to be tested
#' @param na values can be treated as empty, default is NULL
#'
#' @examples
#' x <- c("a", "b", "c", " ", "", NA, "#N/A", "--")
#' isempty(x)
#' isempty(x, "#N/A")
#' isempty(x, c("#N/A", "--"))
#' y <- c(1, 2, 3, 0, 8, NA, 99, NA, 99)
#' isempty(y)
#' isempty(y, 0)
#' isempty(y, c(0, 99))
#' @export
isempty <- function(x, na = NULL) {
    if (length(x) == 0) return(TRUE)
    if (is.null(x)) return(TRUE)
    empty_x <- is.na(x) | x %in% na
    if (is.character(x))
        empty_x <- empty_x | stringr::str_detect(x, "^\\s*$")
    empty_x
}

#' Replacing empty value with latest non-empty value
#'
#' @description based on https://stackoverflow.com/a/13810615
#' @param x an R object to be filled
#' @param forward bool value, define filling direction
#' @param maxgap Int value, define max fill gap
#' @examples
#' x = c(NA,NA,'a',NA,NA,NA,NA,NA,NA,NA,NA,'b','c','d',NA,NA,NA,NA,NA,'e') 
#' fill(x)
#' fill(x, forward = FALSE)
#' fill(x, maxgap = 4)
#' fill(x, maxgap = 5)
#' @export
fill = function(x, forward = TRUE, maxgap = Inf) {
    if (!forward) x = rev(x)                 # reverse x twice if carrying backward
    ind = which(!isempty(x))                 # get positions of empty values
    if (isempty(x[1]))                       # if it begins with NA
        ind = c(1, ind)                       # add first pos
    rep_times = diff(c(ind, length(x) + 1))  # diffing the indices + length yields how often they need to be repeated

    if (maxgap < Inf) {
        exceed = rep_times - 1 > maxgap  # exceeding maxgap
        if (any(exceed)) {               # any exceed?
            ind = sort(c(ind[exceed] + 1, ind))      # add NA in gaps
            rep_times = diff(c(ind, length(x) + 1) ) # diff again
        }
    }
    x = rep(x[ind], times = rep_times) # repeat the values at these indices
    if (!forward) x = rev(x)           # second reversion
    x
}
