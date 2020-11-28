#' expression type form AdvanceR by Hadley Wickham
#'
#' @param x expression
#' @return a string represent the expression type
#' @export
expr_type <- function(x) {
    x <- rlang::enexpr(x)
    if (rlang::is_syntactic_literal(x)) {
        "constant"
    } else if (is.symbol(x)) {
        "symbol"
    } else if (is.call(x)) {
        "call"
    } else if (is.pairlist(x)) {
        "pairlist"
    } else {
        typeof(x)
    }
}


