#' gen regress formula
#'
#' @param dep: dependent variable, require
#' @param indep_ex: exogeneous independent variables, require 
#' @param fe: fixed effect, optional
#' @param cluster: cluster varaible, optional
#' @param indep_in: endogenous independent variables, optional
#' @param iv: Instrumental variables, must longer than `indep_in`
#' @param method: Formula usage scenario, default is felm
#'
#' @export
genFormula <- function(dep, indep_ex, fe = NULL, cluster = NULL,
                       indep_en = NULL, iv = NULL, method = "felm") {
    if (is.null(dep) || !is.character(dep)) stop("dep: character vector")

    if (method == "felm") {
        genFormula.felm(dep, indep_ex, fe, cluster, indep_en, iv)
    }
}

genFormula.felm <- function(dep, indep_ex, fe, cluster, indep_en, iv) {
    k.dep_indep <- paste(dep, "~", paste0(indep_ex, collapse = " + "))
    k.fe <- if (is.null(fe)) {
        "0"
    } else {
        paste0(fe, collapse = " + ")
    }
    k.cluster <- if (is.null(cluster)) {
        "0"
    } else {
        paste0(cluster, collapse = " + ")
    }
    k.indep_in <- if (is.null(indep_in)) {
        "0"
    } else {
        if (length(indep_in) == 1) {
            paste0("\\(", indep_in, " ~ ",
                    paste0(iv, collapse = " + "), "\\)")
        } else {
            paste0("(", paste0(indep_in, collapse = "|"), " ~ ",
                    paste0(iv, collapse = " + "), ")")
        }
    }
    as.formula(paste0(c(k.dep_indep, k.fe, k.indep_in, k.cluster),
                        collapse = " | ")) 
}

