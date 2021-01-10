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
stxtpsm <- function(data, treat, cov, lag = NULL, method = "logit", ...) {
    stopifnot(stxtcheck(data)[[1]])
    matchit_args <- list(...)

    keep_vars <- c(attr(data, "xt"), treat, cov)
    sample <- data[, ..keep_vars]
    data.table::setnames(sample, c("ID", "time", "treat", cov))
    stxtset(sample, "ID", "time")

    class(sample$treat) <- "integer"
    sample[, treat := as.logical(treat)]

    lag %<>% ifthen(0L)
    if (inherits(lag, "integer")) {
        lag <- rep_len(lag, length(cov)) %>% as.list()
        names(lag) <- cov
    }
    for (name in cov) lag[[name]] %<>% ifthen(0L)
    covs <- purrr::map2(names(lag), lag, ~ {
        keep_covs <- vector("list", length(.y))
        for (i in seq_along(.y)) {
            if (.y[i] != 0) stlag(sample, .x, n = as.integer(.y[i]))
            keep_covs[[i]] <- paste0("L", .y[i], ".", .x)
        }
        keep_covs
    }) %>%
    unlist()
    covs <- gsub("^L0\\.", "", covs)
    covs <- gsub("^L1\\.", "L.", covs)

    sample[, treated := treat - ifempty(stlag(treat, time), 0) == 1L, by = "ID"] %>%
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
    sample[, pscore := predict(..esti, type = "response")]

    unbalance <- local({
        sample_filtered <- sample[time == treatStart | is.na(treatStart)]
        c("pscore", covs) %>%
            lapply(diff_check, data = sample_filtered, over = "treat") %>%
            do.call(rbind, .)
    }) %>% setDT()


    if (!require(MatchIt)) stop("Pleas install package MatchIt first")
    sample[, matchID := NA]
    matchit_args$formula <- formula
    treatStartTimes <- na.omit(unique(sample$treatStart))
    matchlog <- vector("list", length(treatStartTimes))
    names(matchlog) <- treatStartTimes

    for (i in seq_along(treatStartTimes)) {
        t <- treatStartTimes[i]
        matchit_args$data <- sample[is.na(matchID) & time == t]
        if (length(unique(matchit_args$data$treat)) != 2) next
        matchit_args$distance <- matchit_args$data$pscore
        matchlog[[i]] <- do.call(MatchIt::matchit, matchit_args)

        match_table <- local({
            m <- matchlog[[i]]
            cID <- matchit_args$data$ID[as.integer(m$match.matrix)]
            tID <- matchit_args$data$ID[as.integer(rownames(m$match.matrix))]
            data.table(
                ID = c(tID, cID),
                match_new = rep(paste(tID, cID, sep = "-"), 2)
            )[!is.na(ID)][is.na(match_new), match_new := ""] 
        })
        sample <- match_table[sample, on = "ID"] %>%
            .[, matchID := ifempty(matchID, match_new)] %>%
            .[, match_new := NULL]
    }
    sample %>%
        setorder(matchID, treatStart, na.last = TRUE) %>%
        .[!lbs::isempty(matchID),
          treatStart := first(treatStart), by = "matchID"]

    balance <- local({
        sample_filtered <- sample[!isempty(matchID) & time == treatStart]
        c("pscore", covs) %>%
            lapply(diff_check, data = sample_filtered, over = "treat") %>%
            do.call(rbind, .)
    }) %>% setDT()

    sample %<>% .[, .(ID, time, treat, treatStart, pscore, matchID)]
    setnames(sample, c("ID", "time"), keep_vars[1:2])
    stxtset(sample, keep_vars[1], keep_vars[2])
    list(data = sample,
         log = matchlog,
         check = list(unbalance = unbalance, balance = balance))
}

diff_check <- function(var, data, over) {
    formula <- as.formula(gettextf("%s ~ %s", var, over))
    t <- t.test(formula, data)    
    out <- data.frame(var, t$estimate[1], t$estimate[2],
                      t$estimate[1] - t$estimate[2],
                      t$stderr, t$p.value)
    names(out) <- c("variable", "group1", "group2", "diff", "diff_sd", "diff_p")
    out
}

#' get treated and control group match table
#'
#' @export
getmatchtable_panel <- function(data, ...) {
    stopifnot(require(MatchIt))
    match_args <- list(...)
    stopifnot(stxtcheck(datadata)[[1]])
    stopifnot(all(c("treatStart", "pscore") %in% names(data)))
    sample <- data[, .SD, .SDcols = c(attr(data, "xt"), "treatStart", "pscore")]
    names(sample) <- c("ID", "time", "treatStart", "pscore")
    sample[, treat := !is.na(treatStart)][, matchID := ""]

    match_args$data <- sample
    match_args$distance <- sample$pscore

    for (t in unique(sample$treatStart)) {
        m <- do.call(MatchIt::matchit, match_args)
    }
}
