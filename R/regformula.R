#' gen regress formula
#'
#' @param dep: dependent variable, require
#' @param indep_ex: exogeneous independent variables, require 
#' @param fe: fixed effect, optional
#' @param cluster: cluster varaible, optional
#' @param en: endogenous independent variables, optional
#' @param iv: Instrumental variables, must longer than `en`
#' @param method: Formula usage scenario, default is felm
#'
#' @export
genformula <- function(varlist, method = NULL, fe = NULL, cluster = NULL,
                       en = NULL, iv = NULL, ...) {
    stopifnot(is.character(varlist))
    stopifnot(length(varlist) >= 2)
    method <- ifthen(method, "linear")
    switch(method,
           linear       = genformula_linear(varlist[1], varlist[-1]),
           felm         = genformula_felm(varlist[1], varlist[-1], fe, cluster, en, iv),
           fixest       = genformula_fixest(varlist[1], varlist[-1], fe, en, iv),
           fixest_sunab = genformula_fixest_sunab(varlist[1], varlist[-1], fe, ...),
           defualt      = print("Not Support Yet")
    )
}

genformula_linear <- function(dep, indep, asstring = FALSE) {
    if (is.null(indep) || !is.character(indep))
        stop("indep need to be a character vector", call. = FALSE)
    f <- paste(paste0(dep, collapse = " + "),
               paste(indep, collapse = " + "),
               sep = " ~ ")
    if (isFALSE(asstring)) f <- as.formula(f)
    return(f)
}

genformula_fixest <- function(dep, indep_ex, fe, en, iv) {
    dep_indep <- genformula_linear(dep, indep_ex, asstring = TRUE)
    fe <- if (is.null(fe))       "" else paste0(fe, collapse = " + ")
    en <- if (is.null(en)) "" else genformula_linear(en, iv, asstring = TRUE)
    f <- c(dep_indep, fe, en)
    f <- paste(f[f != ""], collapse = " | ")
    return(as.formula(f))
}

genformula_felm <- function(dep, indep_ex, fe, cluster, en, iv) {
    dep_indep <- genformula_linear(dep, indep_ex, asstring = TRUE)

    fe <- if (is.null(fe)) "0" else paste0(fe, collapse = " + ")
    cluster <- if (is.null(cluster)) "0" else paste0(cluster, collapse = " + ") 
    en <- if (is.null(en)) {
        "0"
    } else {
        if (length(iv) < length(en))
            stop("Too few instrumental variables", call. = FALSE)
        if (length(en) == 1) {
            paste0("(", en, " ~ ", paste0(iv, collapse = " + "), ")")
        } else {
            paste0("(", paste0(en, collapse = "|"), " ~ ",
                    paste0(iv, collapse = " + "), ")")
        }
    }
    as.formula(paste0(c(dep_indep, fe, en, cluster), collapse = " | ")) 
}

genformula_fixest_sunab <- function(dep, indep, fe, ...) {
    dep        <- as.symbol(dep)
    indep      <- character_vector_to_linear_expression(indep)
    fe         <- character_vector_to_linear_expression(fe)
    sunab_args <- rlang::enexprs(...) %>%
                  purrr::map(~ if (is.character(rlang::expr(!!.x))) as.symbol(.x)
                               else                                 .x)
    sunab      <- as.call(c(quote(sunab), sunab_args))

    if (is.null(indep)) {
        rlang::expr(!!dep ~ !!sunab | !!fe) %>% as.formula()
    } else {
        rlang::expr(!!dep ~ !!indep + !!sunab | !!fe) %>% as.formula()
    }
}

character_vector_to_linear_expression <- function(x) {
    if (is.null(x)) return(NULL)
    purrr::map(x, as.symbol) %>% purrr::reduce(~ rlang::expr(!!.x + !!.y))
}

