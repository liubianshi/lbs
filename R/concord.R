# Fixed routes retain the original module's intermediate classifications.
.concord_paths <- list(
  ISIC3_GB11 = c("ISIC3_GB02", "GB02_GB11"),
  GB11_ISIC3 = c("GB11_GB02", "GB02_ISIC3"),
  GB02_GB17 = c("GB02_GB11", "GB11_GB17"),
  GB17_GB02 = c("GB17_GB11", "GB11_GB02"),
  ISIC3_GB17 = c("ISIC3_GB11", "GB11_GB17"),
  GB17_ISIC3 = c("GB17_GB11", "GB11_ISIC3")
)

.concord_missing <- function() list(match = NA_character_, weight = NA_real_)

.concord_require <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    logger_factory("Concord")$error("Package '", package, "' is required.")
  }
}

.concord_system <- function(x) {
  stopifnot(is.character(x), length(x) == 1L, !is.na(x), nzchar(x))
  toupper(x)
}

# Pad numeric codes to the most frequent observed width, without truncation.
.concord_format_code <- function(code) {
  code <- as.character(code)
  valid <- !is.na(code) & grepl("^[0-9]+$", code)
  if (!any(valid)) return(code)
  widths <- nchar(code[valid])
  sizes <- unique(widths)
  width <- sizes[which.max(tabulate(match(widths, sizes)))]
  code[valid] <- paste0(strrep("0", pmax(0L, width - widths)), code[valid])
  code
}

#' Read an industry classification concordance table
#'
#' Reads Coding_Inds from the data lake selected by `ECON_DATA_LAKE_PATH`,
#' falling back to the reverse table only when the forward file is absent.
#' @param orig Character scalar. Source classification, such as `"GB02"`.
#' @param dest Character scalar. Destination classification, such as `"GB11"`.
#' @return A two-column data.table with character codes, named by classification.
#' @export
get_gbtable <- function(orig, dest) {
  orig <- .concord_system(orig)
  dest <- .concord_system(dest)
  table <- paste(orig, dest, sep = "_")
  path <- file.path(.lake_entity_dir(lake_id("Coding_Inds", table),
                                   .resolve_lake_path()), "data.parquet")
  if (!file.exists(path)) table <- paste(dest, orig, sep = "_")
  data <- lake_read("Coding_Inds", table)
  cols <- match(toupper(paste0(c(orig, dest), "_code")), toupper(names(data)))
  if (anyNA(cols)) logger_factory("Concord")$error("Missing code columns in ", table)
  out <- data.table::as.data.table(lapply(data[, cols, with = FALSE], .concord_format_code))
  data.table::setnames(out, c(orig, dest))
  out
}

.concord_direct <- function(sourcevar, tbl, digit) {
  re <- lapply(sourcevar, \(code) {
    if (is.na(code)) return(.concord_missing())
    hit <- !is.na(tbl[[1]]) & substr(tbl[[1]], 1L, nchar(code)) == code
    matches <- substr(tbl[[2]][hit], 1L, digit)
    if (!length(matches)) return(.concord_missing())
    codes <- unique(matches)
    counts <- tabulate(match(matches, codes), nbins = length(codes))
    list(match = codes, weight = counts / sum(counts))
  })
  names(re) <- sourcevar
  missing <- vapply(re, \(x) all(is.na(x$match)), logical(1)) & !is.na(sourcevar)
  if (any(missing)) logger_factory("Concord")$warning(
    "Matches for code(s): ", paste(unique(sourcevar[missing]), collapse = ", "),
    " not found and returned NA.")
  attr(re, "origin") <- names(tbl)[1L]
  attr(re, "destination") <- names(tbl)[2L]
  re
}

.concord_combine <- function(first, second) {
  stopifnot(identical(attr(first, "destination"), attr(second, "origin")))
  re <- lapply(first, \(entry) {
    branches <- lapply(seq_along(entry$match), \(i) {
      next_entry <- if (is.na(entry$match[i])) NULL else second[[entry$match[i]]]
      if (is.null(next_entry)) next_entry <- .concord_missing()
      list(match = next_entry$match, weight = entry$weight[i] * next_entry$weight)
    })
    matches <- unlist(lapply(branches, `[[`, "match"), use.names = FALSE)
    weights <- unlist(lapply(branches, `[[`, "weight"), use.names = FALSE)
    codes <- unique(matches)
    group <- match(matches, codes)
    list(match = codes, weight = vapply(seq_along(codes),
                                       \(i) sum(weights[group == i]), numeric(1)))
  })
  attr(re, "origin") <- attr(first, "origin")
  attr(re, "destination") <- attr(second, "destination")
  re
}

#' Convert codes between classification systems
#'
#' GB conversions use data-lake tables and row-frequency weights. Other
#' conversions use `concordance::concord()` with `all = TRUE`.
#' @param sourcevar Character vector of codes, retaining leading zeros.
#' @param origin Character scalar. Source classification.
#' @param destination Character scalar. Destination classification.
#' @param dest.digit Positive integer. Destination code width; NULL uses four
#'   digits for GB conversions and the concordance package default otherwise.
#' @return Named list of lists containing character `match` and numeric `weight`
#'   vectors, with uppercase `origin` and `destination` attributes.
#' @export
concord <- function(sourcevar, origin, destination, dest.digit = NULL) {
  stopifnot(is.character(sourcevar))
  orig <- .concord_system(origin)
  dest <- .concord_system(destination)
  if (!is.null(dest.digit)) stopifnot(is.numeric(dest.digit), length(dest.digit) == 1L,
    !is.na(dest.digit), is.finite(dest.digit), dest.digit >= 1, dest.digit == floor(dest.digit))
  if (!startsWith(orig, "GB") && !startsWith(dest, "GB")) {
    .concord_require("concordance")
    re <- if (is.null(dest.digit)) concordance::concord(sourcevar, origin, destination, all = TRUE)
    else concordance::concord(sourcevar, origin, destination, dest.digit, all = TRUE)
    attr(re, "origin") <- orig
    attr(re, "destination") <- dest
    return(re)
  }
  if (is.null(dest.digit)) dest.digit <- 4L
  path <- .concord_paths[[paste(orig, dest, sep = "_")]]
  if (is.null(path)) return(.concord_direct(sourcevar, get_gbtable(orig, dest), dest.digit))
  re <- NULL
  for (i in seq_along(path)) {
    systems <- strsplit(path[i], "_", fixed = TRUE)[[1L]]
    codes <- if (is.null(re)) sourcevar else unique(unlist(lapply(re, `[[`, "match")))
    codes <- codes[!is.na(codes)]
    if (is.null(re)) codes <- sourcevar
    digit <- if (i == length(path)) dest.digit else NULL
    next_re <- concord(as.character(codes), systems[1L], systems[2L], digit)
    re <- if (is.null(re)) next_re else .concord_combine(re, next_re)
  }
  re
}

#' Calculate an index across industry classifications
#'
#' Rate indices sum data values times target-to-data weights. Level indices
#' additionally multiply each contribution by its data-to-target weight.
#' @param sourcevar Character vector of target-classification codes.
#' @param data_code Character scalar. Classification used in query_data.
#' @param target_code Character scalar. Classification used in sourcevar.
#' @param query_data Two-column data.frame or data.table: unique character codes
#'   followed by numeric values. The input is not modified.
#' @param index_type Character scalar, either `"level"` (default) or `"rate"`.
#' @param data_code_digit Positive integer or NULL. Data code width.
#' @param target_code_digit Positive integer or NULL. Target code width.
#' @param na.rm Logical scalar. Remove missing contributions when summing.
#'   Unmatched target codes always return NA.
#' @return Named numeric vector, one value per sourcevar entry.
#' @export
cal_index <- function(sourcevar, data_code, target_code, query_data,
                      index_type = "level", data_code_digit = NULL,
                      target_code_digit = NULL, na.rm = FALSE) {
  stopifnot(is.character(sourcevar), inherits(query_data, "data.frame"),
    ncol(query_data) == 2L, !anyDuplicated(query_data[[1L]]),
    is.character(query_data[[1L]]), is.numeric(query_data[[2L]]),
    length(index_type) == 1L, index_type %in% c("level", "rate"),
    is.logical(na.rm), length(na.rm) == 1L, !is.na(na.rm))
  from <- concord(sourcevar, target_code, data_code, data_code_digit)
  codes <- unique(unlist(lapply(from, `[[`, "match"), use.names = FALSE))
  to <- if (index_type == "level") concord(as.character(codes[!is.na(codes)]),
                                          data_code, target_code, target_code_digit)
  re <- vapply(seq_along(sourcevar), \(i) {
    entry <- from[[i]]
    if (is.na(sourcevar[i]) || all(is.na(entry$match))) return(NA_real_)
    values <- query_data[[2L]][match(entry$match, query_data[[1L]])]
    adjustment <- if (index_type == "rate") 1 else vapply(entry$match, \(code) {
      if (is.na(code)) return(NA_real_)
      back <- to[[code]]
      hit <- which(!is.na(back$match) & back$match == sourcevar[i])
      if (!length(hit)) return(NA_real_)
      sum(back$weight[hit])
    }, numeric(1))
    sum(values * entry$weight * adjustment, na.rm = na.rm)
  }, numeric(1))
  stats::setNames(re, sourcevar)
}
