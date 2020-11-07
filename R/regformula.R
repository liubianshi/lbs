#' gen regress formula
#'
#' @param dep: dependent variable, require
#' @param indep_ex: exogeneous independent variables, require 
#' @param fe: fixed effect, optional
#' @param cluster: cluster varaible, optional
#' @param indep_en: endogenous independent variables, optional
#' @param iv: Instrumental variables, must longer than `indep_en`
#' @param method: Formula usage scenario, default is felm
#'
#' @export
genFormula <- function(dep, indep_ex, fe = NULL, cluster = NULL,
                       indep_en = NULL, iv = NULL, method = "felm", ...) {
    if (is.null(dep) || !is.character(dep))
        stop("dep need to be a character vector")

    if (isTRUE(method == "felm")) {
        genFormula.felm(dep, indep_ex, fe, cluster, indep_en, iv)
    }
}

genFormula.felm <- function(dep, indep_ex, fe, cluster, indep_en, iv) {
    if (length(dep) != 1) stop("dep only take one variable")
    if (is.null(indep_ex) || !is.character(indep_ex))
        stop("indep_ex need to be a character vector")
    dep_indep <- paste(dep, "~", paste0(indep_ex, collapse = " + "))

    fe <- if (is.null(fe)) "0" else paste0(fe, collapse = " + ")
    cluster <- if (is.null(cluster)) "0" else paste0(cluster, collapse = " + ") 
    indep_en <- if (is.null(indep_en)) {
        "0"
    } else {
        if (length(iv) < length(indep_en))
            stop("Too few instrumental variables")
        if (length(indep_en) == 1) {
            paste0("(", indep_en, " ~ ", paste0(iv, collapse = " + "), ")")
        } else {
            paste0("(", paste0(indep_en, collapse = "|"), " ~ ",
                    paste0(iv, collapse = " + "), ")")
        }
    }
    as.formula(paste0(c(dep_indep, fe, indep_en, cluster), collapse = " | ")) 
}
