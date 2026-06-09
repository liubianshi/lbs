#' Custom theme for ggplot: minimal
#'
#' @description A ggplot theme based on `cowplot::theme_minimal_hgrid()` with
#'   font sizes scaled by `dpi`.
#'
#' @param legend.position passed to `ggplot2::theme()`. Default `"top"`.
#' @param dpi integer; output DPI used to scale font sizes. Default `600L`.
#' @param fontfamily font family for all text. Defaults to
#'   `getOption("font.serif")` or `"serif"` if unset.
#' @export
lbs_theme_minimal <- function(legend.position = "top",
                              dpi = 600L,
                              fontfamily = getOption("font.serif")) {
    fontfamily <- ifthen(fontfamily, "serif")
    textsize <- list(
        note   = (dpi / 300) * 8,
        axis   = (dpi / 300) * 9,
        normal = (dpi / 300) * 10,
        stitle = (dpi / 300) * 11,
        title  = (dpi / 300) * 12
    )

    cowplot::theme_minimal_hgrid() +
    ggplot2::theme(
        text                 = ggplot2::element_text(family = fontfamily, size = textsize$normal, colour = "black"),
        aspect.ratio         = 0.618,
        plot.tag             = ggplot2::element_text(size = textsize$note, colour = "black"),
        plot.title           = ggplot2::element_text(hjust = 0.5),
        plot.margin          = ggplot2::margin(1, 1, 1, 1, "mm"),
        axis.text            = ggplot2::element_text(size = textsize$axis, colour = "black"),
        axis.ticks.length    = grid::unit(0.01, "mm"),
        axis.title.x         = ggplot2::element_blank(),
        axis.text.x          = ggplot2::element_text(angle = 0, vjust = 0.5),
        axis.ticks.x         = ggplot2::element_line(linewidth = 0.5),
        axis.ticks.length.x  = grid::unit(2, "mm"),
        panel.grid.major.y   = ggplot2::element_line(linewidth = 0.2, colour = "grey85"),
        panel.grid.major.x   = ggplot2::element_blank(),
        panel.grid.minor     = ggplot2::element_blank(),
        legend.text          = ggplot2::element_text(size = textsize$note, hjust = 0),
        legend.title         = ggplot2::element_blank(),
        legend.key.size      = grid::unit(0.8, "cm"),
        legend.key.height    = grid::unit(0.4, "cm"),
        legend.box.spacing   = grid::unit(0, "cm"),
        legend.box.margin    = ggplot2::margin(b = 0.5, unit = "cm"),
        legend.margin        = ggplot2::margin(r = 1, unit = "cm"),
        legend.spacing.x     = grid::unit(0.2, "cm"),
        legend.box           = "horizontal",
        legend.justification = "center",
        legend.position      = legend.position
    )
}



