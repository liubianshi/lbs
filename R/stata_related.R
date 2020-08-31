#' des variables
#'
#' @description Describe the basic information of variables in stata-style
#' 
#' @param df `data.frame`
#' @examples
#' df <- data.frame(a = 1:3, b = 2:4)
#' des(df)
#'
#' attr(df$a, "label") = "A"
#' attr(df$b, "label") = "B"
#' des(df)
#' @export
des <- function(df) {  #> 载入自定义函数
    get_type_label <- function(x) {
        if ("label" %in% names(attributes(x))) 
            c(typeof(x), attr(x, "label"))
        else c(typeof(x), "")
    }
    label <- sapply(df, get_type_label)
    label <- t(label)
    label <- as.data.frame(label)
    colnames(label) <- c("type", "label")
    label$variable <- rownames(label)
    rownames(label) = NULL
    subset(label, select = c("variable", "type", "label"))
}


