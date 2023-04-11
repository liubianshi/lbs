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

    sample <- match_table[sample, on = "ID"] %>%
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
        sample[, (lag$names) := cal_lag_of(.SD[[..name]], Time, ..lag$lags), by = "ID"]
    })
    cov_names <- purrr::map(covs, "names") %>% unlist()

    keep_vars <- c("ID", "Time", "Treat", "TreatStart", cov_names)
    keep_times <- unique(sample$TreatStart)
    sample <- sample[, ..keep_vars]                   %>% 
         .[!Treat | Time <= TreatStart]               %>% # drop observation treated more than one period
         .[Time %in% keep_times]                      %>% # drop time without treated individual
         na.omit(c("ID", "Time", "Treat", cov_names))

    propensity_score <- cal_propensity_score(sample, "Treat", cov_names, method)
    sample$pscore <- propensity_score$result

    setattr_formatch(sample, propensity_score$formula, cov_names, method)
}

cal_lag_of <- function(var, time, lags) {
    lags <- as.integer(lags)
    lapply(lags, function(l) if (l == 0) var else stlag(var, time, l))
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
                             ifelse(.y == 1, paste0("L.", .x),
                                             paste0("L", .y, ".", .x))),
                     lags  = .y)
            })
    names(covs) <- cov_names
    covs
}

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

match_by_treat_start_date <- function(data, args) {
    if (!require(MatchIt)) stop("Pleas install package MatchIt first")
    stopifnot(inherits(data, "datatable_for_match"))
    args$formula  <- attr(data, "pscore_formula")
    args$mahvars  <- args$formula[-2] # remote dependent variable from formula
    args$caliper  <- ifthen(args$caliper, 0.05)

    match_table   <- data.table(ID = unique(data$ID), matchID = NA)
    match_by_time <- sort(na.omit(intersect(data$Time, data$TreatStart)))

    match_results <- purrr::map(match_by_time, function(t) {
        keep <- data$Time == t                               &
                data$ID %in% match_table[is.na(matchID), ID] &
                (!data$Treat | (data$Treat & data$TreatStart == t))
        sample <- data[keep] 
        rownames(sample) <- NULL 

        match_result <- tryCatch(
            psm(sample, "ID", "Treat", "pscore", args),
            error = function(cond) {
                if (grepl("No units", cond)) {
                    message(gettextf("Time %d: %s", t, cond$message))
                    return(NULL)
                }
                stop(cond)
            }
        )

        if (is.null(match_result)) {
            return(NULL)
        }

        match_table <<- match_result$match_table %>%
            .[match_table, on = "ID"] %>%
            .[, .(ID, matchID = ifelse(is.na(i.matchID), matchID, i.matchID))]

        message(gettextf(
            "Time %d: Number of obs.:  %d (original), %d (matched)", t,
            nrow(match_result$X),
            nrow(na.omit(match_result$match.matrix))
        ))
        match_result
    })
    names(match_results) <- paste("Started Treated at", match_by_time)
    list(result = match_table, log = match_results)
}

setattr_formatch <- function(data, formula, covnames, method) {
    stopifnot(inherits(data, "data.table"))
    stopifnot(setequal(names(data), c("ID", "Time", "Treat", "TreatStart", "pscore", covnames)))
    setcolorder(data, c("ID", "Time", "Treat", "TreatStart", "pscore", covnames))
    data.table::setattr(data, "covariates", covnames)
    data.table::setattr(data, "pscore_formula", formula)
    data.table::setattr(data, "pscore_method", method)
    data.table::setattr(data, "class", c("datatable_for_match", class(data)))
}
