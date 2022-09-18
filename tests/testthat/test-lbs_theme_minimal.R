context("Test ggplot2 theme")

test_that("Test lbs_theme_miminal", {
    dt <- data.table::as.data.table(mtcars)
    stlabel(dt, wt, "测试")
    p <- ggplot2::ggplot(dt, ggplot2::aes(qsec, wt)) +
        ggplot2::geom_point() +
        ggplot2::scale_y_continuous(name = "测试") +
        lbs_theme_minimal()
    expect_equal(p$theme$legend.position, "top")
    #expect_true(p$theme$text$family %in% c("serif", "NotoSerif"))
})
