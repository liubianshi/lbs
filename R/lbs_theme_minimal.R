#' Custom theme for ggplot: minimal
#'
#' @title: ggplot theme, based on `cowplot` 的 `theme_minimal_grid`
#'
#' @export
lbs_theme_minimal <- function(legend.position = "top",
                              dpi = 600L,
                              fontfamily = getOption("font.serif")) {
    fontfamily %<>% ifthen("serif")
    textsize <- list(
        note   = (dpi / 100) * 8,
        axis   = (dpi / 100) * 9,
        normal = (dpi / 100) * 10,
        stitle = (dpi / 100) * 11,
        title  = (dpi / 100) * 12
    )

    cowplot::theme_minimal_hgrid() +
    ggplot2::theme(
        text                 = element_text(family = fontfamily, size = textsize$normal, colour = "black"),
        aspect.ratio         = 0.618,
        plot.title           = element_text(hjust = 0.5),
        plot.margin          = margin(1, 1, 1, 1, "mm"),
        axis.text            = element_text(size = textsize$axis, colour = "black"),
        axis.ticks.length    = unit(0.01, "mm"),
        axis.title.x         = element_blank(),
        axis.text.x          = element_text(angle = 0, vjust = 0.5),
        axis.ticks.x         = element_line(size = 0.5),
        axis.ticks.length.x  = unit(2, "mm"),
        panel.grid.major.y   = element_line(size = 0.2, colour = "grey85"),
        panel.grid.major.x   = element_blank(),
        panel.grid.minor     = element_blank(),
        legend.text          = element_text(size = textsize$note, hjust = 0),
        legend.title         = element_blank(),
        legend.key.size      = unit(0.8, "cm"),
        legend.key.height    = unit(0.4, "cm"),
        legend.box.spacing   = unit(0, "cm"),
        legend.box.margin    = margin(b = 0.5, unit = "cm"),
        legend.margin        = margin(r = 1, unit = "cm"),
        legend.spacing.x     = unit(0.2, "cm"),
        legend.box           = "horizontal",
        legend.justification = "center",
        legend.position      = legend.position
    )
}



