#' print auxiliary table for flextable object in order to use pandoc-crossref
#' 
#'  Used in Rmarkdown like file with `results = "asis"`
#' 
#' @param ft a flextable object
#' @param caption a string for table caption
#' @param label a valid name for pondoc-crossref
#' @export
print_flextable <- function(ft, caption = NULL, label = NULL) {
    label   %<>% ifthen(knitr::opts_current$get("label"))
    caption %<>% ifthen(knitr::opts_current$get("fig.cap"))
    cat(gettextf("Table: %s {\\#tbl:%s}\n\n", caption, label))
    print(knitr::kable(data.frame(test = "test"), "simple"))
    ft
}
