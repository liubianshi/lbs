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

#' execute propensity match and return match result object with match table
#'
#' @param sample a data.table object, one line per individual 
#' @param id name of a column used to identify individual
#' @param treat name of a column used to identify treated status
#' @param pscore name of a column for propensity score
#' @param args a list contain element to be passed to `MatchIt::matchid`
#'
#' @export
psm <- function(sample, id, treat, pscore, args) {
    stopifnot(all(c(id, treat, pscore) %in% names(sample)))
    stopifnot(!anyDuplicated(sample[[id]]))
    rownames(sample) <- NULL
    args$data <- sample
    args$distance <- args$data[[pscore]]
    match_result <- do.call(MatchIt::matchit, args)

    control_list <- sample[[id]][as.integer(match_result$match.matrix)]
    treated_list <- sample[[id]][as.integer(rownames(match_result$match.matrix))]
    match_result$match_table <- data.table(
        ID      = c(treated_list, control_list),
        matchID = rep(paste(treated_list, control_list, sep = "-"), 2)
    )[!(is.na(ID) | grepl("NA", matchID))]

    match_result
}

#' calculate propensity score for panel data
#'
#' @param data a data.table labeld as panel data by `stxtset()`
#' @param treat variable name that indicates whether the unit is treated
#' @param cov character vector indicating the set of covarieates
#' @param lag an integer vector indicating how to use lags of covarieates
#' @param method a string indicating the method used to calcuate propensity
#'        score which will be passed to `binomial()` as augument `link`.
#' @return a list with match result, match log and balance check result
#' @export
stxtpsm <- function(data, treat, cov, lag  = NULL, id     = NULL,
                                      time = NULL, method = "logit", ...) {
    matchit_args <- list(...)

    # check id and time setting
    id   <- ifthen(id,   attr(data, "xt")[1])
    time <- ifthen(time, attr(data, "xt")[2])
    stopifnot(!is.null(id) && !is.null(time))
    stopifnot(!anyDuplicated(data[, c(id, time)]))

    # prepare standardized data for match
    sample          <- prepare_sample_for_match(data, id, time, treat, cov, lag, method)
    match_cov_names <- attr(sample, "covariates")
    match_result    <- match_by_treat_start_date(sample, matchit_args)
    match_table     <- match_result$result
    match_log       <- match_result$log

    sample <-
        match_table[sample, on = "ID"] %>%
        setorder(matchID, -TreatStart) %>%
        .[!isempty(matchID), TreatStart := last(TreatStart), by = "matchID"]
    
    balance_check <-
        list(before_match = sample[!Treat | (Treat & Time == TreatStart)],
             after_match  = sample[!isempty(matchID) & Time == TreatStart]) %>%
        lapply(diff_between_treat_control, treat  = "Treat",
                                           covs   = match_cov_names,
                                           pscore = "pscore")

    sample %<>% .[, .(ID, Time, Treat, TreatStart, pscore, matchID)]
    setnames(sample, c("ID", "Time", "Treat", "TreatStart"),
                     c(id,    time,   treat,   paste0(treat, "_start")))
    stxtset(sample, id, time)
    list(data  = sample, log = match_log, check = balance_check)
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

prepare_sample_for_match <- function(data, id, time, treat, cov, lag = NULL, method = "logit") {
    # extract needed variable
    keep_vars <- c(id, time, treat, cov)
    sample <- data.table::as.data.table(data)[, ..keep_vars]
    setnames(sample, c(id, time, treat), c("ID", "Time", "Treat"))

    # group individuals by first treated date
    sample[, TreatStart := cal_treated_start_time(Time, Treat), by = "ID"]
    sample[, Treat      := is_treated_group(Treat),             by = "ID"]

    # cal cov lagged
    covs <- standardize_cov_lag(cov, lag)
    covs %>% purrr::iwalk(function(lag, name) {
        lags     <- lag$lags$lag
        cal_mean <- lag$lags$mean
        sample[, (lag$names) := cal_lag_of(.SD[[..name]], Time, ..lags, ..cal_mean), by = "ID"]
    })
    cov_names  <- purrr::map(covs, "names") %>% unlist()

    keep_vars  <- c("ID", "Time", "Treat", "TreatStart", cov_names)
    keep_times <- unique(sample$TreatStart)
    sample     <- sample[, ..keep_vars]                   %>%
                  .[!Treat | Time <= TreatStart]          %>% # drop observation treated more than one period
                  .[Time %in% keep_times]                 %>% # drop time without treated individual
                  na.omit(c("ID", "Time", "Treat", cov_names))

    propensity_score <- cal_propensity_score(sample, "Treat", cov_names, method)
    sample$pscore <- propensity_score$result

    setattr_formatch(sample, propensity_score$formula, cov_names, method)
}

cal_lag_of <- function(var, time, lags, mean = FALSE) {
    lags <- as.integer(lags)
    vars <- lapply(lags, function(l) if (l == 0) var else stlag(var, time, l))
    if (length(vars) > 1 & isTRUE(mean)) {
        vars <- purrr::reduce(vars, `+`) / length(vars)
    }
    vars
}

standardize_cov_lag <- function(cov_names, lag_list = NULL) {
    lag_list <- standarise_lag_with_covs(lag_list, cov_names)
    purrr::imap(lag_list, function(lag, cov_name) {
        if (length(lag$lag) > 1 & isTRUE(lag$mean)) {
            list(names = paste0("M.", cov_name), lags = lag)
        } else {
            list(names = ifelse(lag$lag == 0, cov_name,
                         ifelse(lag$lag == 1, paste0("L.", cov_name),
                                              paste0("L", lag$lag, ".", cov_name))),
                 lags  = lag)
        }
    })
}

#' calculate the time when an individual was first treated
#' @param time time
#' @param treat whether treated at specific time
#'
#' @export
cal_treated_start_time <- function(time, treat) {
    time <- as.integer(time)
    treat <- as.logical(treat)
    L.treat <- stlag(treat, time, 1L) %>% ifempty(0)
    treatStart <- if (max(treat, na.rm = TRUE) == 0) {
        NA_integer_ 
    } else {
        time[treat - L.treat == 1] %>% min(na.rm = TRUE)
    }
    treatStart
}

is_treated_group <- function(treat) {
    max(treat != 0, na.rm = TRUE) %>% as.logical()
}

cal_propensity_score <- function(data, treat, covs, method = "logit") {
    stopifnot(all(c(treat, covs) %in% names(data)))
    formula <- as.formula(gettextf("%s ~ %s", treat, paste(covs, collapse = " + ")))
    esti <- glm(formula, data = data, family = binomial(link = method))
    list(result = predict(esti, type = "response"), formula = formula)
}

diff_between_treat_control <- function(data, treat, covs, pscore = NULL) {
    if (nrow(data) == 0L) return(NULL)
    if (!is.null(pscore)) {
        covs <- c(pscore, covs)
    }
    lapply(covs, diff_check, data = data, over = treat) %>%
    do.call(rbind, .) %>%
    setDT() %>%
    .[, variable := ..covs]
}

get_vars_from_formula <- function(fml) {
    if (is.name(fml)) return(as.character(fml))
    unlist(lapply(fml[-1], get_vars_from_formula))
}

match_by_treat_start_date <- function(data, args, breaks = NULL) {
    if (!require(MatchIt)) stop("Pleas install package MatchIt first")
    stopifnot(inherits(data, "datatable_for_match"))

    args$formula  <- attr(data, "pscore_formula")
    args$mahvars  <- args$formula[-2] # remote dependent variable from formula
    args$caliper  <- ifthen(args$caliper, 0.05)
    covs <- get_vars_from_formula(args$mahvars)

    start_time_groups <- local({
        group_time_with_breaks  <- function(t, b, m = min(t), M = max(t)) {
            if (m == M || is.null(b)) return(as.list(t))
            b <- c(m, b[b %between% t], M) %>% unique() %>% sort()
            purrr::map2(b[-length(b)], b[-1], ~ if (.x == b[1]) t[t >= .x & t <= .y]
                                                else            t[t >  .x & t <= .y])
        }
        start_times <- sort(na.omit(intersect(data$Time, data$TreatStart)))
        r <- group_time_with_breaks(start_times, na.omit(breaks))
        names(r) <- purrr::map_chr(r, ~ paste("Treat Start:", paste(.x, collapse = ", ")))
        r
    })

    sample_groups <- purrr::map(start_time_groups, function(start_times){
        data[
            i       = { times <- get("start_times", parent.frame(n = 3))
                        Time %in% times & (!Treat | (Treat & TreatStart %in% times)) },
            j       = c(lapply(.SD, mean, na.rm = TRUE)),
            by      = c("ID", "Treat"),
            .SDcols = c("pscore", covs)
        ]
    })

    results <- local({
        match_table   <- data.table(ID = unique(data$ID), matchID = NA)
        update_match_table <- function(sample, info) {
            individuals_not_yet_matched <- match_table[is.na(matchID), ID]
            sample <- sample[ID %in% individuals_not_yet_matched]
            match_result <- tryCatch(
                psm(sample, "ID", "Treat", "pscore", args),
                error = function(cond) {
                    if (grepl("No units", cond)) {
                        message(gettextf("%s: %s", info, cond$message))
                    } else stop(cond)
                }
            )
            if (is.null(match_result)) return(NULL)
            match_info <- gettextf("%s: \n\tNumber of obs.: %d (original), %d (matched)\n",
                                   info,
                                   nrow(match_result$X),
                                   nrow(na.omit(match_result$match.matrix)))
            message(match_info)

            matchID_for_update <- do.call(
                function(id, table, names = c("ID", "matchID")) {
                    table[[names[2]]][match(id, table[[names[1]]])]
                },
                list(id = match_table$ID, table = match_result$match_table)
            )
            match_table[, matchID := ifelse(is.na(matchID), ..matchID_for_update, matchID)]

            match_result
        }
        match_results <- purrr::imap(sample_groups, ~ update_match_table(.x, .y))
        list(result = match_table, log = match_results)
    })
}

setattr_formatch <- function(data, formula, covnames, method) {
    stopifnot(inherits(data, "data.table"))
    stopifnot(setequal(names(data), c("ID", "Time", "Treat", "TreatStart", "pscore", covnames)))
    setcolorder(data, c("ID", "Time", "Treat", "TreatStart", "pscore", covnames))
    data %>% data.table::setattr("covariates",     covnames) %>%
             data.table::setattr("pscore_formula", formula)  %>%
             data.table::setattr("pscore_method",  method)   %>%
             data.table::setattr("class",          c("datatable_for_match", class(data)))
    data
}

standarise_lag_with_covs <- function(lag_list, covs) {
    stopifnot(is.null(lag_list) || is.numeric(lag_list) || is.list(lag_list))
    if (is.null(lag_list) || is.numeric(lag_list)) {
        lag_list <- purrr::map(covs, ~ standarise_lag(lag_list))
    } else {
        lag_default <- list(lag = 0L, mean = FALSE) 
        if ("mean" %in% names(lag_list)) {
            lag_default <- list(lag = 0L, mean = lag_list$mean) 
            lag_list$mean <- NULL
        }
        if (is.null(names(lag_list)) || all(names(lag_list) == "")) {
            if (length(lag_list) == 1L) {
                lag_list <- purrr::map(covs, ~ standarise_lag(lag_list[[1]], lag_default))
            } else if (length(lag_list) == length(covs)) {
                lag_list <- purrr::map2(lag_list, standarise_lag, lag_default = lag_default)
            } else {
                stop("length of `lag` not equal to length of `covs`", call. = FALSE)
            }
        } else {
            lag_list <- purrr::map(covs, ~ standarise_lag(lag_list[[.x]], lag_default))
        }
    }
    names(lag_list) <- covs
    lag_list
}

standarise_lag <- function(lag = NULL, lag_default = list(lag = 0L, mean = FALSE)) {
    lag_names <- names(lag_default)
    error_message <- "lag setting error!"
    if (is.list(lag) && length(lag) > 2L)
        stop(error_message, call. = FALSE)
    if (length(setdiff(names(lag), lag_names)) != 0L &&
        any(setdiff(names(lag), lag_names) != ""))
        stop(error_message, call. = FALSE)

    if (length(lag) == 0L) lag <- lag_default
    if (is.numeric(lag))   lag <- list(lag, lag_default[[2]])
    
    if (length(lag) == 1L) {
        if (is.null(names(lag))) {
            lag <- list(lag[[1]], lag_default[[2]])
        } else {
            miss_name <- setdiff(lag_names, names(lag))
            lag[[miss_name]] <- lag_default[[miss_name]]
        }
    }
    if (is.null(names(lag))) {
        names(lag) <- lag_names
    } 
    if (any(names(lag) == "")) {
        names(lag)[names(lag) == ""] <- setdiff(lag_names, names(lag))
    }
    lag
}


# vim: foldmethod=syntax
