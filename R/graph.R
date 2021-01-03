#' generate scale transform function pair
#'
#' @param x a numeric vector with two elements
#' @param y a numeric vector with two elements
#' @return a list contain two reciprocal functions
#'
#' @export
trans_scale_factory <- function(x, y) {
    stopifnot(all(length(x) == 2L, length(y) == 2L))
    stopifnot(all(x[2] > x[1], y[2] > y[1]))
    multi <- (y[2] - y[1]) / (x[2] - x[1])
    list(
         forword  = function(z) (z - x[1]) * multi + y[1],
         backword = function(z) (z - y[1]) / multi + x[1]
    )
}

#' set default graph elements
#'
#' @export
setgraphelements <- function(
    dpi = 600L,
    linetype  = "solid",
    linecolor = "black",
    fontcolor = "black",
    fontfamily = "Noto Serif CJK SC"
) {
    multi <- dpi / 100
    settext <- function(size, bold = FALSE) {
        size <- multi * size
        face <- if (isTRUE(bold)) "bold"
        ggplot2::element_text(family = fontfamily, size = size,
                     color = fontcolor, face = face)
    }
    setline <- function(size, color = linecolor, linetype = linetype) {
        ggplot2::element_line(color, size, linetype)
    }

    text <- list(
        note   = settext(8),
        axis   = settext(9),
        normal = settext(10),
        stitle = settext(11),
        title  = settext(12, bold = TRUE)
    )
    line <- list(
        grid_minor = setline(0.2, colorspace::lighten(linecolor, 0.2)),
        grid_major = setline(0.3, colorspace::lighten(linecolor, 0.3)),
        thin       = setline(0.5),
        normal     = setline(1),
        thick      = setline(1.5),
        vthick     = setline(2),
        vvthick    = setline(4)
    )
    margin <- list(
        zero   = ggplot2::margin(0, 0, 0, 0),
        tiny   = ggplot2::margin(2, 2, 2, 2, unit = "mm"),
        small  = ggplot2::margin(5, 5, 5, 5, unit = "mm"),
        normal = ggplot2::margin(1, 1, 1, 1, unit = "cm"),
        big    = ggplot2::margin(2, 2, 2, 2, unit = "cm"),
        huge   = ggplot2::margin(3, 3, 3, 3, unit = "cm")
    )
    list(text = text, line = line, margin = margin)
}


