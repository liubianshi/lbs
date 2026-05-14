#' execute pre-trend test for did analysis
#'
#' @param data a data.frame / data.table with panel observations.
#' @param id column name identifying the cross-sectional unit.
#' @param time column name identifying the time period.
#' @param output column name of the outcome variable.
#' @param treat_status optional column name of a binary treatment indicator.
#'   Either `treat_status` or `group` must be supplied.
#' @param group optional column name storing the first-treatment cohort.
#' @param never_treat value in `group` representing never-treated units; rows
#'   with this value are recoded to `NA`.
#' @param covs character vector of additional covariate column names.
#' @param drop_exposure_list integer vector of relative-time dummies to drop
#'   (reference category). Default `-1`.
#' @export
event_study_result <- function(data, id, time, output,
                               treat_status = NULL,
                               group = NULL,
                               never_treat = NULL,
                               covs = NULL,
                               drop_exposure_list = -1)
{
    stopifnot(!is.null(treat_status) | !is.null(group))
    sample <- as.data.table(data) %>%
        .[, .SD, .SDcols = c(id, time, group, treat_status, output, covs)] %>%
        na.omit(c(id, time, output, covs))
    setnames(sample, c(id, time), c("ID", "Time"))
    if (!is.null(covs)) {
        setnames(sample, covs, paste0("cov_", covs))
        covs <- paste0("cov_", covs)
    }
    if (is.null(group)) {
        setnames(sample, treat_status, "Treat")
        sample[, Group := cal_treated_start_time(Time, Treat), by = "ID"]
        sample[, Treat := NULL]
    } else {
        setnames(sample, group, "Group")
    }
    if (!is.null(never_treat) && !is.na(never_treat)) {
        sample[Group == never_treat, Group := NA]
    }
    exposure_dummy <- generate_exposure_dummy_list(sample$Time, sample$Group, drop_exposure_list)
    covs_coef      <- estimate_exposure_coef(data  = cbind(sample, exposure_dummy),
                                             dep   = output,
                                             indep = c(covs, names(exposure_dummy)),
                                             fe    = c("ID", "Time"))

    complete_exposure_list <-
        complete_exposure(covs_coef$var) %>%
        as.data.table(keep.rownames = TRUE) %>%
        setnames(c("var", "exposure"))
    complete_exposure_dummy_coefs <-
        covs_coef[complete_exposure_list, on = "var", nomatch = NA] %>%
        .[is.na(coef), `:=`(coef = 0, se = 0)] %>%
        setcolorder(c("var", "exposure", "coef", "se"))
}

generate_exposure_dummy_list <- function(time, treat_time,
                                         drop_exposure_list = -1,
                                         name_prefix = c("Pre", "Treat", "Post"))
{
    T_max      <- max(time, na.rm = TRUE)
    T_min      <- min(time, na.rm = TRUE)
    T_len      <- T_max - T_min + 1

    exposure_list        <- setdiff((-T_len):(T_len-1), drop_exposure_list)
    names(exposure_list) <- naming_exposure_list(exposure_list, name_prefix) 

    dummies <- purrr::map(
        exposure_list,
        ~ {
            exposure_dummy <- ifelse(is.na(treat_time), FALSE, time - treat_time == .x)
            if (all(!exposure_dummy)) NULL else exposure_dummy
        }
    )
    dummies <- purrr::compact(dummies)
    as.data.frame(dummies, check.names = FALSE, stringsAsFactors = FALSE)
}


estimate_exposure_coef <- function(data, dep, indep, fe, se = "cluster") {
    fml <- lbs::genformula(c(dep, indep), "fixest", fe)
    es <- fixest::feols(fml, data, se = se)
    print(summary(es))
    data.table(var = stringr::str_replace(names(es$coefficients), "TRUE$", ""),
               coef = es$coefficients,
               se = es$se)
}

complete_exposure <- function(exposure, prefix = c("Pre", "Treat", "Post")) {
    stopifnot(length(prefix) == 3L)
    names(prefix) <- c("pre", "cur", "post")
    min <- stringr::str_match(exposure, paste0(prefix["pre"], "(\\d+)"))[,2]  %>%
        as.integer() %>%
        max(na.rm = TRUE)
    max <- stringr::str_match(exposure, paste0(prefix["post"], "(\\d+)"))[,2] %>%
        as.integer() %>%
        max(na.rm = TRUE)
    complete_exposure_list <- (-min):max
    names(complete_exposure_list) <- naming_exposure_list(complete_exposure_list, prefix)
    complete_exposure_list
}

naming_exposure_list <- function(exposure, prefix = c("Pre", "Treat", "Post")) {
    stopifnot(length(prefix) == 3L)
    names(prefix) <- c("pre", "cur", "post")
    exposure  <- as.integer(exposure)
    treat_period_name_prefix <- ifelse(exposure <  0, prefix["pre"],
                                ifelse(exposure == 0, prefix["cur"],
                                                          prefix["post"]))
    paste0(treat_period_name_prefix, abs(exposure))
}

gen_example_data_for_did <- function(time_max       = 60,
                                     id_max         = 30,
                                     treat_wave_num = 5,
                                     cov_num        = 2,
                                     seed           = 123456L)
{
    set.seed(seed)
    data <-
        expand.grid(time = 1:time_max, id = 1:id_max) %>%
        data.table::setDT() %>%
        .[, cohort := sample.int(..treat_wave_num, 1), by = "id"] %>%
        .[, first_treat := ifelse(cohort == sample(cohort, 1), Inf, cohort)] %>%
        .[first_treat > max(time), first_treat := Inf] %>%
        .[, `:=`(treat = (time >= first_treat), rel_time = time - first_treat)]
    if (cov_num > 0) {
        data[, (paste0("cov_", 1:cov_num)) :=
               lapply(1:cov_num, rnorm, n = max(id) * max(time))]
    }

    rel_times <- unique(data[rel_time >=0, rel_time])
    effects <- rnorm(1) + rel_times
    names(effects) <- rel_times

    data[, error := rnorm(max(id) * max(time))] %>%
       .[, y := id + time +
                ifelse(rel_time >= 0, ..effects[as.character(rel_time)], 0) +
                purrr::reduce(.SD, `+`),
           .SDcols = patterns("^(cov_|error$)")]
    data[, `:=`(cohort = NULL, rel_time = NULL, error = NULL)]
    setcolorder(data, c("id", "time", "y", "treat", "first_treat"))
    data
}

#' draw event study ploy with event study coefficients
#'
#' @param coef_result a data.frame with columns `exposure`, `coef`, `se`
#'   (typically the output of `event_study_result()`).
#' @param xlabel x-axis label.
#' @param ylabel y-axis label.
#' @export
event_study_plot <- function(coef_result, xlabel = "Exposure",
                                   ylabel = "Coefficient Estimate") {
    ggplot(data = coef_result, aes(y = coef, x = exposure)) +
        geom_hline(yintercept = 0, alpha = 0.4, linewidth = 1.2) +
        geom_line(linetype = "dashed") +
        geom_point() +
        geom_errorbar(aes(ymin = (coef - 1.96 * se), 
                          ymax = (coef + 1.96 * se)), width = 0.2) +
        scale_x_continuous(name = xlabel,
                           breaks = coef_result$exposure,
                           minor_breaks = NULL) +
        scale_y_continuous(name = ylabel,
                           n.breaks = 8,
                           expand = expansion(0.1), 
                           minor_breaks = NULL) +
        lbs_theme_minimal() +
        theme(axis.title.x = element_text(margin = margin(t = 5)))
}

