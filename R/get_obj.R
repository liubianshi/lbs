#' Get variable names from data.frame or list
#'
#' @param data a data.frame or list
#' @param ... symbol, character vector of names
#' @examples
#' get_df_names(mtcars, mpg, cyl, disp, hp)
#' get_df_names(mtcars, c("mpg", "cyl", "disp", "hp"))
#' x <- c("mpg", "cyl", "disp", "hp")
#' get_df_names(mtcars, x)
#' x <- c("mpg", "cyl", "disp", "hp2")
#' try(get_df_names(mtcars, x))
#' @export
get_df_names <- function(data, ...) {
    vars <- rlang::enquos(...)
    nms <- names(data)
    names(nms) <- names(data)
    varnames <- purrr::map(vars, rlang::eval_tidy, as.list(nms))
    for (i in seq_along(varnames)) {
        if (is.numeric(varnames[[i]]))
            varnames[[i]] <- nms[varnames[[i]]] 
    }
    varnames <- purrr::flatten_chr(varnames)
    if (is.numeric(varnames)) varnames <- names(data)[varnames]
    stopifnot(all(varnames %in% names(data)))
    varnames
}
