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
    stopifnot(!is.null(attr(data, "xt")))
    matchit_args <- list(...)
    keep_vars <- c(attr(data, "xt"), treat, cov)

    sample <- data[, ..keep_vars]
    data.table::setnames(sample, c("ID", "time", "treat", paste0("cov", seq_along(cov))))
    stxtset(sample, "ID", "time")
    setpaneltreat(sample, treat)
    covs <- setcovlags(sample, cov, lag)
    sample %<>%
        .[!treat | time <= treatStart] %>%
        .[time %in% unique(sample$treatStart)]
        na.omit(c("ID", "time", "treat", covs))

    formula <- as.formula(gettextf("treat ~ %s", paste(covs, collapse = " + ")))
    esti <- glm(formula, data = sample, family = binomial(link = method))
    sample[, pscore := predict(..esti, type = "response")]

    unbalance <- local({
        sample_filtered <- sample[!treat | (treat & time == treatStart)]
        c("pscore", covs) %>%
            lapply(diff_check, data = sample_filtered, over = "treat") %>%
            do.call(rbind, .) %>%
            setDT() %>%
            .[, variable := c("Propensity Score", names(covs))]
    })

    if (!require(MatchIt)) stop("Pleas install package MatchIt first")
    sample[, matchID := NA]
    matchit_args$formula <- formula
    treatStartTimes <- intersect(
        na.omit(unique(sample$treatStart)),
        na.omit(unique(sample$time))
    ) %>% sort()
    matchlog <- vector("list", length(treatStartTimes))
    names(matchlog) <- paste("Treated time:", treatStartTimes)

    match_result <- lapply(treatStartTimes, psm, sample, matchit_args)

    psm <- function(tStart, sample, args) {
        neededvars <- c("ID", "time", "treat", "treatStart", "matchID")
        stopifnot(all(neededvars %in% names(data)))

        args$data <- local({
            keep <- sample$time == tstart & (
                (sample$treat & sample$treatStart == tStart ) |
                (!sample$treat & is.na(sample$matchID))
            )
            sample[keep]
        })
    }

    for (i in seq_along(treatStartTimes)) {
        matchit_args$data <- local({
            keep <- sample$time == treatStartTimes[i] & (
                (sample$treat  & sample$treatStart == treatStartTimes[i] ) |
                (!sample$treat & is.na(sample$matchID))
            )
            sample[keep]
        })
        matchit_args$distance <- matchit_args$data$pscore

        matchlog[[i]] <- try(do.call(MatchIt::matchit, matchit_args), silent = TRUE)
        if (class(matchlog[[i]]) == "try-error") {
            if (!grepl("No units", matchlog[[i]])) stop(matchlog[[i]])
            next
        }

        match_table <- local({
            m <- matchlog[[i]]
            cID <- matchit_args$data$ID[as.integer(m$match.matrix)]
            tID <- matchit_args$data$ID[as.integer(rownames(m$match.matrix))]
            data.table(
                ID = c(tID, cID),
                match_new = rep(paste(tID, cID, sep = "-"), 2)
            )[!is.na(ID)][grepl("NA", match_new), match_new := ""]
        })

        sample <- match_table[sample, on = "ID"] %>%
            .[, matchID := ifelse(is.na(matchID), match_new, matchID)] %>%
            .[, match_new := NULL]
    }

    sample %>%
        setorder(matchID, treatStart, na.last = TRUE) %>%
        .[!isempty(matchID),
          treatStart := first(treatStart), by = "matchID"]

    balance <- local({
        sample_filtered <- sample[!isempty(matchID) & time == treatStart]
        if (nrow(sample_filtered) != 0) {
            c("pscore", covs) %>%
                lapply(diff_check, data = sample_filtered, over = "treat") %>%
                do.call(rbind, .) %>%
                setDT()
        } else {
            NULL
        }
    })

    sample %<>% .[, .(ID, time, treat, treatStart, pscore, matchID)]
    setnames(sample, c("ID", "time"), keep_vars[1:2])
    stxtset(sample, keep_vars[1], keep_vars[2])
    list(data = sample,
         log = matchlog,
         check = list(unbalance = unbalance, balance = balance))
}

# check statistics different
diff_check <- function(var, data, over) {
    formula <- as.formula(gettextf("%s ~ %s", var, over))
    t <- t.test(formula, data)    
    out <- data.frame(var, t$estimate[1], t$estimate[2],
                      t$estimate[1] - t$estimate[2],
                      t$stderr, t$p.value)
    names(out) <- c("variable", "group1", "group2", "diff", "diff_sd", "diff_p")
    out
}

# set lagged cov and return all covs
setcovlags <- function(data, cov, lag, label) {
    label %<>% ifthen(cov)
    lag %<>% ifthen(0L)
    if (is.numeric(lag))
        lag <- rep_len(as.integer(lag), length(cov)) %>% as.list()
    if (is.null(names(lag)) && length(lag) != length(cov))
        stop("If lag is a list without names, the length must equal cov!")
    if (is.null(names(lag))) names(lag) <- cov
    for (name in cov) lag[[name]] %<>% ifthen(0L)

    covs <- vector("list", length(lag))
    for (i in seq_along(lag)) {
        var  <- names(lag)[i]
        lags <- lag[[i]]
        covs[[i]] <- purrr::map(lags, function(l) {
            if (l != 0) stlag(data, var, n = as.integer(l))
            v = paste0("L", l, ".", var)
            names(v) <- paste0("L", l, ".", var)
            v
        })
    }
    covs %<>%
        unlist() %>%
        gsub("^L0\\.", "", .) %>%
        gsub("^L1\\.", "L.", .)
    names(covs) %<>%
        gsub("^L0\\.", "", .) %>%
        gsub("^L1\\.", "L.", .)
    covs
}

# set treat related variables for panel data
setpaneltreat <- function(data) {
    stopifnot(all(c("ID", "time", "treat") %in% names(data)))
    class(data$treat) <- "integer"
    data[, treated := treat - ifempty(stlag(treat, time), 0) == 1L, by = "ID"] %>%
        .[(treated), treatStart := time] %>%
        setorder(ID, treatStart, na.last = TRUE) %>%
        .[, treatStart := first(treatStart), by = "ID"] %>%
        .[, treated := NULL] %>%
        setorder(ID, treat) %>%
        .[, treat := as.logical(last(treat)), by = "ID"] %>%
        setorder(ID, time)
    data
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
