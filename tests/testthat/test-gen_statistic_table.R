context("Test generate statistic table")

test_that("Test gen_statistic_table", {
    stat <- data.table::fread("
        0.3016 0.3110 0.3070 0.3086
        0.6742 0.6112 0.6365 0.6268
        0.5324 0.4721 0.4963 0.4870
        0.5830 0.5948 0.5605 0.5752
        0.3059 0.2438 0.2780 0.2525
    ") %>% as.data.frame()
    p <- data.table::fread("
        0.050459 0.046868 0.0463999 0.046275
        0.001648 0.002496 0.0019996 0.002160
        0.004155 0.006482 0.0051930 0.005621
        0.005267 0.004958 0.0061344 0.005523
        0.000252 0.006780 0.0009388 0.004277
    ") %>% as.data.frame()
    t <- stat
    gen_statistic_table(stat, p, t = t, format = c(t = "(2)", p = "[3]")) %>% print()
})


