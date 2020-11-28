#' check whether a data.frame is a panel table
#'
#' @param df data.frame object
#' @export
stxtcheck <- function(df) {
    xt <- attr(df, "xt")
    if (is.null(xt)) 
        return(list(FALSE, message = "Not set attribute xt yet"))
    if (length(xt) != 2)
        return(list(FALSE, message = "attribute xt can only contain two varname"))
    if (!is.character(xt))
        return(list(FALSE, message = "attribute xt need to be a character vector"))
    if (!all(xt %in% names(df)))
        return(list(FALSE, message = "attribute xt contain varaible not in data.frame"))
    if (!is.integer(df[[xt[2]]]))
        return(list(FALSE, message = "time variable must point to an integer vector"))
    id_time <- data.table(id = df[[xt[1]]], time = df[[xt[2]]])
    if (anyDuplicated(id_time[!is.na(time)]) != 0) {
        return(list(FALSE, message = paste(
            "exists duplicates by", xt[1], "and", xt[2]
        )))
    }
    return(list(TRUE, message = paste("The data.table is a panel table")))
}

#' set data.frame as panel data
#'
#' @description Mark `data.table` as Panel data, and set `id` and `time`
#' @param data a data.table object
#' @param id variable name as panel id
#' @param time variable name as panel time
#' @export
stxtset <- function(df, id, time) {
    id <- rlang::enquo(id)
    time <- rlang::enquo(time)
    id_time_name <- get_df_names(df, !!id, !!time)
    setattr(df, "xt", id_time_name)

    check_result <- stxtcheck(df)
    if (isFALSE(check_result[[1]])) {
        stop("panel set failed:\n", check_result[[2]])
    }
    invisible(df)
}
