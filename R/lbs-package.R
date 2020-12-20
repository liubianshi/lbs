#' Personal use fucntion
#'
#' @name lbs-package
#' @aliases lbs
#' @docType package
NULL

.onLoad <- function(libname, pkgname) {
    NULL
}

# Import -----------------------------------------------

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom data.table :=
#' @export
data.table::`:=`

#' @importFrom data.table setattr
#' @export
data.table::setattr

#' @importFrom data.table setDT
#' @export
data.table::setDT

#' @importFrom data.table data.table
#' @export
data.table::data.table

#' @importFrom data.table as.data.table
#' @export
data.table::as.data.table

#' @importFrom rlang !!
#' @export
rlang::`!!`

#' @importFrom rlang !!!
#' @export
rlang::`!!!`

# regforuse -------------------------------------------------------------------

