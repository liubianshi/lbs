.onLoad <- function(libname, pkgname) {
  NULL
}

#' @importFrom data.table ":="
NULL

utils::globalVariables(c(
  # data.table NSE operators and specials
  ".", ".SD", ".I", ".N", ".BY", ".GRP",
  # data.table ..var parent-frame lookups
  "..cal_mean", "..covs", "..databaseList", "..effects",
  "..k.lag.varlist", "..keep_vars", "..keys", "..lags",
  "..matchID_for_update", "..n", "..name", "..tableList", "..treat_wave_num",
  # data.table infix
  "%between%",
  # column name NSE references
  "coef", "cohort", "error", "exposure", "first_treat", "Group",
  "id", "ID", "label", "m", "M", "matchID", "no", "pscore", "randno",
  "rel_time", "se", "time", "Time", "Treat", "treat_no", "TreatStart",
  "variable", "y"
))
