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
    if (length(x) == 0) return(x)
    empty_x <- is.na(x) | x %in% na
    if (is.character(x))
        empty_x <- empty_x | stringr::str_detect(x, "^\\s*$")
    empty_x
}

