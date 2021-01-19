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
    keep_vars    <- c(attr(data, "xt"), treat, cov)
    sample <- data[, ..keep_vars]
    data.table::setnames(sample, c("ID", "time", "treat", paste0("cov", seq_along(cov))))
    stxtset(sample, "ID", "time")
    setpaneltreat(sample)
    covs <- setcovlags(sample, paste0("cov", seq_along(cov)), lag, label = cov)
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
    if (is.null(names(lag))) names(lag) <- label
    for (name in label) lag[[name]] %<>% ifthen(0L)

    covs <- vector("list", length(lag))
    for (i in seq_along(lag)) {
        nm  <- names(lag)[i]
        va  <- cov[match(nm, label)]
        la  <- lag[[i]]
        covs[[i]] <- lapply(la, function(l) {
            if (l != 0) stlag(data, va, n = as.integer(l))
            v = paste0("L", l, ".", va)
            names(v) <- paste0("L", l, ".", nm)
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

# set tmereat related variables for panel data
setpaneltreat <- function(data) {
    stopifnot(all(c("ID", "time", "treat") %in% names(data)))
    setattr(data$treat, "class", "integer")
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
getmatchtable_panel <- function(data, ...) {s
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

# perform propensity matching
psm <- function(sample, tStart, args, covlabel = NULL) {
    neededvars <- c("ID", "time", "treat", "treatStart", "matchID", "pscore")
    stopifnot(all(neededvars %in% names(sample)))

    args$data <- local({
        keep <- sample$time == tStart & (
            (sample$treat & sample$treatStart == tStart) |
            (!sample$treat & is.na(sample$matchID))
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
