#' check whether a data.frame is a time-series table
#'
#' @param dt data.frame object
#' @export
sttscheck <- function(df) {
    ts <- attr(df, "ts")
    if (is.null(ts)) 
        return(list(FALSE, message = "Not set attribute ts yet"))
    if (length(ts) != 1)
        return(list(FALSE, message = "attribute ts can only contain one varname"))
    if (!is.character(ts))
        return(list(FALSE, message = "attribute ts need to be a character vector"))
    if (!all(ts %in% names(df)))
        return(list(FALSE, message = "attribute ts not in data.frame"))
    if (!is.integer(df[[ts]]))
        return(list(FALSE, message = "time variable must point to an integer vector"))
    if (anyDuplicated(df[[ts]], incomparables = NA) != 0) {
        return(list(FALSE, message = paste("exists duplicates by", ts)))
    }
    return(list(TRUE, message = paste("The data.frame is a time-series table")))
}

#' set data.frame as time-series data
#'
#' @description Mark `data.table` as Panel data, and set `id` and `time`
#' @param data a data.table object
#' @param time variable name as time
#' @export
sttsset <- function(df, time) {
    time_name <- gsub("[\"']", "", deparse(substitute(time)))
    if (! time_name %in% names(df)) time_name <- time

    setattr(df, "ts", time_name)
    check_result <- sttscheck(df)
    if (isFALSE(check_result[[1]])) {
        stop("time-series set failed:\n", check_result[[2]])
    }
    invisible(df)
}
