.journal_require <- function(package = "GridTable") {
  if (!requireNamespace(package, quietly = TRUE)) {
    logger_factory("Journal")$error("Package '", package, "' is required.")
  }
}

.journal_validate_models <- function(models, model_class = "fixest") {
  stopifnot(is.list(models), all(vapply(models, inherits, logical(1), what = model_class)))
}

.journal_escape <- function(x, chars) {
  for (char in unique(strsplit(chars, "", fixed = TRUE)[[1L]])) {
    x <- gsub(char, paste0("\\", char), x, fixed = TRUE)
  }
  x
}

#' Translate dependent variable names using a dictionary
#' @param dep_var_names Character vector of variable names.
#' @param translation_dict Named list or character vector. Missing entries retain the original name.
#' @return Character vector of translated names.
#' @export
map_dependent_variable_names <- function(dep_var_names, translation_dict) {
  stopifnot(is.character(dep_var_names))
  vapply(dep_var_names, \(name) {
    i <- match(name, names(translation_dict))
    if (is.na(i) || is.null(translation_dict[[i]])) name else translation_dict[[i]]
  }, character(1), USE.NAMES = FALSE)
}

#' Extract dependent variable names from regression models
#' @param regression_list List of model objects.
#' @param model_class Character scalar. Required model class, default `"fixest"`.
#' @return Character vector of dependent variable expressions.
#' @export
get_dependent_variable_names <- function(regression_list, model_class = "fixest") {
  .journal_validate_models(regression_list, model_class)
  vapply(regression_list, \(model) {
    formula <- if (inherits(model, "fixest")) stats::formula(model, type = "linear")
    else stats::formula(model)
    deparse1(formula[[2L]])
  }, character(1))
}

#' Extract unique coefficient names from fixest models
#' @param regression_list List of fixest models.
#' @return Character vector of coefficient names in first-appearance order.
#' @export
get_coefficient_names <- function(regression_list) {
  .journal_validate_models(regression_list)
  unique(as.character(unlist(lapply(regression_list, \(model) rownames(model$coeftable)))))
}

#' Look up variable labels
#' @param label_map Named list or character vector of labels.
#' @param var_names Character vector of names, or NULL to derive from models.
#' @param regression_list Optional list of fixest models.
#' @param sort_labels Logical scalar. Sort by dictionary order; unmatched entries
#'   are omitted when sorting, as in the original module.
#' @return Named character vector of labels. Unmatched entries are NA when not sorting.
#' @export
get_variable_labels <- function(label_map, var_names = NULL, regression_list = NULL,
                                sort_labels = FALSE) {
  stopifnot(!(is.null(var_names) && is.null(regression_list)))
  if (is.null(var_names)) var_names <- get_coefficient_names(regression_list)
  positions <- match(var_names, names(label_map))
  if (isTRUE(sort_labels)) positions <- sort(positions)
  labels <- vapply(positions, \(position) {
    if (is.na(position) || is.null(label_map[[position]])) NA_character_
    else label_map[[position]]
  }, character(1), USE.NAMES = FALSE)
  stats::setNames(labels, names(label_map)[positions])
}

#' Convert a knitr table to a formatted GridTable
#' @param kable_string A knitr_kable object in simple or pipe format.
#' @param header_rows Positive integer. Number of header rows, default 2.
#' @param columns_to_bind List of column ranges; NULL merges columns after the
#'   first in row one, and FALSE disables merging.
#' @param table_label Character scalar or NULL to use the current knitr label.
#' @param escape_parentheses Logical scalar. Escape parentheses in header row two.
#' @param default_alignment Character scalar, one of l, c, r. Alignment after column one.
#' @return A GridTable object.
#' @export
enhance_kable_for_gridtable <- function(kable_string, header_rows = 2L,
    columns_to_bind = NULL, table_label = NULL, escape_parentheses = TRUE,
    default_alignment = "c") {
  .journal_require()
  .journal_require("knitr")
  stopifnot(length(header_rows) == 1L, !is.na(header_rows), header_rows >= 1L,
            header_rows == floor(header_rows), length(default_alignment) == 1L,
            default_alignment %in% c("l", "c", "r"))
  if (is.null(table_label)) table_label <- knitr::opts_current$get("label")
  if (length(table_label) && nzchar(table_label)) {
    kable_string <- sub("^(Table: .*)$", paste0("\\1 {#", table_label, "}"),
                        kable_string, perl = TRUE)
  }
  tbl <- GridTable::kable_to_grid(kable_string, header = header_rows)
  stopifnot(header_rows <= nrow(tbl))
  GridTable::set_attr(tbl, "align", paste0("l", strrep(default_alignment, ncol(tbl) - 1L)))
  if (is.null(columns_to_bind)) {
    columns_to_bind <- if (ncol(tbl) > 2L) list(seq.int(2L, ncol(tbl))) else list()
  }
  if (!isFALSE(columns_to_bind)) {
    for (cols in columns_to_bind) GridTable::merge_cells(tbl, 1L, cols)
  }
  if (isTRUE(escape_parentheses) && header_rows >= 2L) {
    for (j in seq_len(ncol(tbl))[-1L]) {
      data.table::set(tbl, 2L, j, .journal_escape(tbl[[j]][2L], "()"))
    }
  }
  tbl[]
}

.journal_rows <- function(tbl, coef_rows) {
  if (is.null(coef_rows)) {
    start <- attr(tbl, "header") + 1L
    end <- attr(tbl, "footer") - 1L
    coef_rows <- if (start <= end) seq.int(start, end) else integer()
  }
  stopifnot(is.numeric(coef_rows), !anyNA(coef_rows), all(is.finite(coef_rows)),
    all(coef_rows == floor(coef_rows)), all(coef_rows >= 1L & coef_rows <= nrow(tbl)),
    !anyDuplicated(coef_rows), length(coef_rows) %% 2L == 0L)
  coef_rows
}

.journal_common <- function(tbl, adjust_width) {
  stopifnot(inherits(tbl, "GridTable"), is.logical(adjust_width),
            length(adjust_width) == 1L, !is.na(adjust_width))
  first <- as.character(tbl[[1L]])
  hit <- which(!is.na(first) & startsWith(first, "$"))
  if (length(hit)) data.table::set(tbl, hit, 1L, paste0("\\", first[hit]))
  if (isTRUE(adjust_width)) {
    widths <- attr(tbl, "width")
    for (merged in attr(tbl, "merged_cells")) {
      cols <- seq.int(merged$cols[1L], merged$cols[2L])
      if (length(cols) <= 1L) next
      minimum <- min(widths[cols], na.rm = TRUE)
      if (!is.finite(minimum)) next
      hit <- cols[!is.na(widths[cols]) & widths[cols] >= minimum * 2 - 2]
      # Never widen a column when applying a width-reduction heuristic.
      widths[hit] <- pmin(widths[hit], minimum + 1)
    }
    if (length(widths)) GridTable::set_attr(tbl, width = widths)
  }
  footer <- attr(tbl, "footer")
  if (is.null(footer) || !is.finite(footer)) {
    blanks <- which(first == "")
    footer <- if (length(blanks)) max(blanks) + 1L else nrow(tbl) + 1L
    GridTable::set_attr(tbl, footer = footer)
  }
  tbl
}

#' Apply journal formatting to a GridTable
#'
#' Modifies the input by reference. World Economy retains separate coefficient
#' and uncertainty rows; Economic Research merges each pair in model columns.
#' @param grid_table A GridTable object.
#' @param coef_rows Numeric vector of row indices in estimate/uncertainty pairs,
#'   or NULL for rows between header and footer.
#' @param model_cols Numeric vector of model column indices, or NULL for all but the first.
#' @param adjust_width Logical scalar. Reduce excessively wide merged columns.
#' @param journal_style Character scalar, `"world_economy"` or `"economic_research"`.
#' @return The GridTable object, modified by reference.
#' @export
format_gtable_by_journal <- function(grid_table, coef_rows = NULL, model_cols = NULL,
    adjust_width = TRUE, journal_style = "world_economy") {
  .journal_require()
  journal_style <- match.arg(journal_style, c("world_economy", "economic_research"))
  stopifnot(inherits(grid_table, "GridTable"))
  if (is.null(model_cols)) model_cols <- seq_len(ncol(grid_table))[-1L]
  stopifnot(is.numeric(model_cols), !anyNA(model_cols),
    all(model_cols == floor(model_cols)), all(model_cols >= 1L & model_cols <= ncol(grid_table)),
    !anyDuplicated(model_cols))
  .journal_common(grid_table, adjust_width)
  rows <- .journal_rows(grid_table, coef_rows)
  if (journal_style == "economic_research") {
    for (j in model_cols) for (i in seq_len(length(rows) %/% 2L)) {
      pair <- rows[c(2L * i - 1L, 2L * i)]
      stopifnot(diff(pair) == 1L)
      same <- vapply(attr(grid_table, "merged_cells"), \(m) {
        identical(as.numeric(m$rows), as.numeric(range(pair))) &&
          identical(as.numeric(m$cols), as.numeric(c(j, j)))
      }, logical(1))
      if (!any(same)) GridTable::merge_cells(grid_table, pair, j, wrap = TRUE)
    }
  }
  grid_table[]
}

#' Translate specific Chinese terms to English within a character vector
#'
#' Replaces predefined Chinese terms with English equivalents using a dictionary.
#'
#' @param text_vector A character vector containing terms to translate.
#' @return A character vector with Chinese terms replaced by English equivalents.
#' @export
#' @examples
#' terms <- c("国家时间: 2023", "金额：1000", "权重: 是")
#' translate_table_terms_to_english(terms)
#' # Expected output: c("Country-Year: 2023", "Value: 1000", "Weight: Y")
#'
#' mixed_terms <- c("这是一个测试，包含国家产品和数量", "另一个测试，无")
#' translate_table_terms_to_english(mixed_terms)
#' # Expected output: c("这是一个测试，包含Country-Product和Quantity", "另一个测试，--")
translate_table_terms_to_english <- function(text_vector) {
  # Ensure the input is a character vector
  stopifnot(is.character(text_vector))

  # Define the translation dictionary (Chinese term = English translation)
  # Using a named vector for efficient replacement with str_replace_all
  # fmt: skip
  translation_dict <- c(
    "\u56fd\u5bb6\u65f6\u95f4" = "Country-Year",
    "\u4ea7\u54c1\u65f6\u95f4" = "Product-Year",
    "\u884c\u4e1a\u65f6\u95f4" = "Product-Year", # Note: Same translation as 产品时间
    "\u56fd\u5bb6\u4ea7\u54c1" = "Country-Product",
    "\u56fd\u5bb6\u884c\u4e1a" = "Country-Product", # Note: Same translation as 国家产品
    "\u88ab\u89e3\u91ca\u53d8\u91cf" = "Dependent Variable",
    "\u56e0\u53d8\u91cf" = "Dependent Variable", # Note: Same translation as 被解释变量
    "\u89c2\u6d4b\u6570" = "Observations",
    "\u6743\u91cd" = "Weight",
    "\u65e0" = "--",
    "\u662f" = "Y",
    "\u5426" = "N",
    "\u91d1\u989d" = "Value",
    "\u6570\u91cf" = "Quantity",
    "\u91cd\u91cf" = "Quantity", # Note: Same translation as 数量
    "\uff1a" = ": " # Replace full-width colon with ASCII colon and space
  )
  # fmt: on

  # Perform all replacements simultaneously using the named vector
  translated_text_vector <- stringr::str_replace_all(text_vector, translation_dict)

  # Return the translated vector
  return(translated_text_vector)
}
