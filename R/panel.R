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

#' calculate propensity score for panel data
#' 
#' @param data a data.table labeld as panel data by `stxtset()`
#' @param treat variable name that indicates whether the unit is treated
#' @param cov character vector indicating the set of covarieates
#' @param lag an integer vector indicating how to use lags of covarieates
#' @param method a string indicating the method used to calcuate propensity
#'        score which will be passed to `binomial()` as augument `link`.
#' @export
calpscore_panel <- function(data, treat, cov, lag = 0L, method = "logit") {
    stopifnot(stxtcheck(data)[[1]])

    keep_vars <- c(attr(data, "xt"), treat, cov)
    covs <- paste0("cov", seq_along(cov))
    newnames <- c("ID", "time", "treat", covs)
    sample <- data[, ..keep_vars]
    data.table::setnames(sample, newnames)

    class(sample$treat) <- "integer"
    sample[, treat := as.logical(treat)]
    stxtset(sample, "ID", "time")

    for (i in lag) {
        if (i == 0L) next
        stlag(sample, covs, n = i)
    }
    covs <- names(sample)[4:length(sample)]

    sample[, treated := treat - ifempty(stlag(treat, time), 0) == 1L,
             by = "ID"] %>%
        .[(treated), treatStart := time] %>%
        setorder(ID, treatStart, na.last = TRUE) %>%
        .[, treatStart := first(treatStart), by = "ID"] %>%
        .[, treated := NULL] %>%
        setorder(ID, time)

    sample <- sample[!(treat & time > treatStart)] %>%
        na.omit(c("ID", "time", "treat", covs))

    formula <- as.formula(gettextf(
        "treat ~ %s", paste(covs, collapse = " + ")
    ))

    esti <- glm(formula, data = sample, family = binomial(link = method))
    out <- sample[, pscore := predict(..esti, type = "response")] %>% 
        .[, .(ID, time, pscore)]
    setnames(out, c("ID", "time"), keep_vars[1:2])
    out
}

