#' check whether a data.frame is a panel table --------------------------------
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
    id           <- rlang::enquo(id)
    time         <- rlang::enquo(time)
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
stxtpsm <- function(data, treat, cov,
                    lag    = NULL,
                    id     = NULL,
                    time   = NULL,
                    method = "logit",
                    ...) {
    stopifnot(!is.null(attr(data, "xt")))
    matchit_args <- list(...)

    id   <- ifthen(id,   attr(data, "xt")[1])
    time <- ifthen(time, attr(data, "xt")[2])
    stopifnot(!is.null(id) && !is.null(time))
    if (anyDuplicated(data[, c(id, time)])) {
        stop("Duplicated!", call. = FALSE)
    }

    sample <- gen_sample_for_match(data, id, time, treat, cov, lag)
    covs <- setdiff(names(sample), c("ID", "Time", "Treat", "TreatStart"))
    sample$pscore <- cal_propensity_score(sample, "Treat", covs, method)

    unbalance_diff <- sample[!Treat | (Treat & Time == TreatStart)] %>%
                      diff_between_treat_control("Treat", covs, "pscore")

    sample$pscore <- identify_matched_group(sample,
                                            id = "ID",
                                            time = "Time",
                                            treat = "Treat",
                                            treat_start = "TreatStart",
                                            covs = covs,
                                            args = matchit_args)

    if (!require(MatchIt)) stop("Pleas install package MatchIt first")
    sample[, matchID := NA]
    matchit_args$formula <- formula
    treatStartTimes <- intersect(
        na.omit(unique(sample$treatStart)),
        na.omit(unique(sample$time))
    ) %>% sort()
    matchlog <- lapply(treatStartTimes, psm, sample = sample, matchit_args, names(covs))
    names(matchlog) <- paste("Treated time:", treatStartTimes)
    setorder(sample, matchID, -treatStart) %>%
        .[!isempty(matchID), treatStart := last(treatStart), by = "matchID"]

    balance <- local({
        sample_filtered <- sample[!isempty(matchID) & time == treatStart]
        if (nrow(sample_filtered) != 0) {
            c("pscore", covs) %>%
                lapply(diff_check, data = sample_filtered, over = "treat") %>%
                do.call(rbind, .) %>%
                setDT() %>%
                .[, variable := c("Propensity Score", names(covs))]
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
    out <- data.frame(var,
                      t$estimate[1],
                      t$estimate[2],
                      t$estimate[1] - t$estimate[2],
                      t$stderr,
                      t$p.value)
    names(out) <- c("variable", "group1", "group2", "diff", "diff_sd", "diff_p")
    out
}

# set lagged vari and return all variables
set_vari_lags <- function(data, vari, lag = NULL, label = NULL) {
    label %<>% ifthen(vari)
    lag   %<>% ifthen(0L)

    # set lag for every variable
    if (is.numeric(lag))
        lag <- rep_len(as.integer(lag), length(vari)) %>% as.list()
    if (is.null(names(lag)) && length(lag) != length(vari))
        stop("If lag is a list without names, the length must equal vari!")
    if (is.null(names(lag))) names(lag) <- label
    for (name in label) lag[[name]] %<>% ifthen(0L)

    # cal lagged variables and return all lagged variable names
    lag_vari <- vector("list", length(lag))
    for (i in seq_along(lag)) {
        nm  <- names(lag)[i]
        la  <- lag[[i]]
        va  <- vari[match(nm, label)]
        lag_vari[[i]] <- lapply(la, function(l) {
            if (l != 0) stlag(data, va, n = as.integer(l))
            v = paste0("L", l, ".", va)
            names(v) <- paste0("L", l, ".", nm)
            v
        })
    }
    lag_vari %<>%
        unlist() %>%
        gsub("^L0\\.", "", .) %>%
        gsub("^L1\\.", "L.", .)
    names(lag_vari) %<>%
        gsub("^L0\\.", "", .) %>%
        gsub("^L1\\.", "L.", .)
    lag_vari
}

# set tmereat related variables for panel data
setpaneltreat <- function(data) {
    stopifnot(all(c("ID", "time", "treat") %in% names(data)))
    setattr(data$treat, "class", "integer")
    # treatStart: the year from untreated state to treated state
    data[, treated := treat - ifempty(stlag(treat, time), 0) == 1L, by = "ID"] %>%
        .[(treated), treatStart := time] %>%
        setorder(ID, treatStart, na.last = TRUE) %>%
        .[, treatStart := first(treatStart), by = "ID"] %>%
        .[, treated := NULL]
    data[, treat := as.logical(max(treat, na.rm = TRUE)), by = "ID"]
    setorder(data, ID, time)
    data
}

#' get treated and control group match table
#'
#' @export
getmatchtable_panel <- function(data, ...) {
    stopifnot(require(MatchIt))
    match_args <- list(...)
    stopifnot(stxtcheck(data)[[1]])
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

# perform propensity matching
psm <- function(sample, tStart, args, covlabel = NULL) {
    neededvars <- c("ID", "time", "treat", "treatStart", "matchID", "pscore")
    stopifnot(all(neededvars %in% names(sample)))

    args$data <- local({
        keep <- sample$time == tStart & (
            (sample$treat & sample$treatStart == tStart) |
            (!sample$treat & is.na(sample$matchID))  # no replacement match
        )
        sample[keep]
    })
    args$distance <- args$data$pscore
    log <- tryCatch(
        do.call(MatchIt::matchit, args),
        error = function(cond) {
            if (grepl("No units", cond)) return(cond$message)
            stop(cond)
        }
    )

    match_new <- local({
        cID <- args$data$ID[as.integer(log$match.matrix)]
        tID <- args$data$ID[as.integer(rownames(log$match.matrix))]
        data.table(ID = c(tID, cID),
                    match_new = rep(paste(tID, cID, sep = "-"), 2)) %>%
        .[!is.na(ID)] %>%
        .[grepl("NA", match_new), match_new := ""] %>%
        .[sample, on = "ID"] %>%
        .[, match_new]
    })

    sample[, matchID := ifelse(is.na(matchID), ..match_new, matchID)]
    if (!is.null(log$X) && !is.null(covlabel))
        names(log$X) <- covlabel
    return(log)
}

gen_sample_for_match <- function(data, id, time, treat, cov, lag = NULL) {
    # extract needed variable
    keep_vars <- c(id, time, treat, cov)
    sample <- data.table::as.data.table(data)[, ..keep_vars]
    setnames(data, c(id, time, treat), c("ID", "Time", "Treat"))

    # group id
    sample[, TreatStart := cal_treated_start_time(Time, Treat), by = "ID"]
    sample[, Treat      := is_treated_group(Treat),             by = "ID"]

    # cal cov lagged
    purrr::iwalk(
        standardize_cov_lag(cov, lag), # list(cov = list(names = , lags = ))
        ~ sample[, (.x$names) := cal_lag_of(...y, Time, ...x$lags)]
    )

    cov_names <- purrr::map(covs, "names") %>% unlist()
    keep_vars <- c("ID", "Time", "Treat", "TreatStart", cov_names)
    keep_times <- unique(sample$treatStart)
    sample[, ..keep_vars]                             %>% 
         .[!Treat | Time <= TreatStart]               %>% # drop observation treated more than one period
         .[Time %in% keep_times]                      %>% # drop time without treated individual
         na.omit(c("ID", "Time", "Treat", cov_names))
}

cal_lag_of <- function(var, time, lags) {
    lapply(lags, function(l) if (l == 0) var else stlag(var, time, l)
}

standardize_cov_lag <- function(cov_names, lag_list = NULL) {
    lag_list %<>% ifthen(0L)
    stopifnot(is.numeric(lag_list) || is.list(lag_list))
    if (is.numeric(lag_list))
        lag_list <- rep_len(as.integer(lag_list), length(cov_names)) %>% as.list()
    if (is.null(names(lag_list)) && length(lag_list) != length(cov_names))
        stop("If lag is a list without names, the length must equal cov!")

    if (is.null(names(lag_list))) names(lag_list) <- cov_names
    covs <- purrr::map2(cov_names, lag_list, ~ {
                list(names = ifelse(.y == 0, .x,
                             ifelse(.y == 1, paste0("L1.", .x),
                                             paste0("L", .y, ".", .x))),
                     lags  = .y)
            })
    names(covs) <- cov_names
    covs
}

cal_treated_start_time <- function(time, treat) {
    setattr(treat, "class", "integer")
    setattr(time, "class", "integer")
    L.treat <- stlag(treat, time, 1L) %>% ifempty(0)
    treatStart <- time[treat - L.treat == 1] %>% min(na.rm = TRUE)
    treatStart
}

is_treated_group <- function(treat) {
    max(treat != 0, na.rm = TRUE) %>% as.logical()
}

cal_propensity_score <- function(data, treat, covs, method = "logit") {
    stopifnot(all(c(treat, covs) %in% names(data)))
    esti <- gettextf("%s ~ %s", treat, paste(covs, collapse = " + ")) %>%
            as.formula() %>%
            glm(data = data, family = binomial(link = method))
    predict(esti, type == "response")
}

diff_between_treat_control <- function(data, treat, covs, pscore = NULL) {
    if (!is.null(pscore)) {
        covs <- c(pscore, covs)
    }
    lapply(covs, diff_check, data = data, over = treat) %>%
    do.call(rbind, .) %>%
    setDT() %>%
    .[, variable := c("Propensity Score", names(covs))]
}

identify_matched_group <- function(data, id, time, treat, treat_start, pscore, args) {
    if (!require(MatchIt)) stop("Pleas install package MatchIt first")

    psm <- function(sample, id, time, treat, treat_start, pscore, args) {
        stopifnot(all(c(id, time, treat, treat_start, pscore), names(sample)))
        args$data <- sample
        args$distance <- args$data[[pscore]]
        match_result <- tryCatch(
            do.call(MatchIt::matchit, args),
            error = function(cond) {
                if (grepl("No units", cond)) return(cond$message)
                stop(cond)
            }
        )
        match_table <- match_result$match.matrix

        match_new <- data.table(ID      = c(rownames(match_table)), match_table),
                                matchID = rep(paste(rownames(match_table), match_table, sep = "-"), 2) %>%
                    .[!is.na(ID) & !grep("NA", matchID)] %>%
                    .[data.table(ID = rownmes(sample)), match_new, on = "ID"]


    sample[, matchID := ifelse(is.na(matchID), ..match_new, matchID)]
    if (!is.null(log$X) && !is.null(covlabel))
        names(log$X) <- covlabel
    return(log)
    }
    
    match_group <- intersect(data[[time]], data[[treat_start]]) %>%
                   na.omit() %>%
                   sort()



    matchlog <- lapply(treatStartTimes, psm, sample = sample, matchit_args, names(covs))
    names(matchlog) <- paste("Treated time:", treatStartTimes)
    setorder(sample, matchID, -treatStart) %>%
        .[!isempty(matchID), treatStart := last(treatStart), by = "matchID"]

}




