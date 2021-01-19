context("Test functions for pam+did analysise")

test_that("Test calculate propensity score for panel data", {
              data <- as.data.table(mtcars)
              data[, ID := rep(1:8, each = 4)] %>%
                  .[, year := rep(2000:2003, 8)] %>%
                  .[, treat := year >= (ID %% 4) + 2002]
              stxtset(data, "ID", "year")
              result <- stxtpsm(data, "treat", c("mpg", "cyl"), 1L)
              expect_equal(dim(result$data), c(14, 6))
              expect_equal(names(result$data)[5:6], c("pscore", "matchID"))
              expect_true(all(between(result$data$pscore, 0, 1)))
              expect_false(anyNA(result$data$pscore))
              expect_equal(anyDuplicated(result$data[1:2]), 0L)
              expect_equal(result$check$balance$variable[1], "Propensity Score")

              print(result)
              result2 <- stxtpsm(data, "treat", c("mpg", "cyl"), lag= list(mpg = c(0,1), cyl = 1),
                                 method = "probit")
              expect_equal(dim(result2$data), c(14, 6))
              expect_equal(dim(result2$check$balance), c(4, 6))
})

#> test_that("Test complex psm", {
#>     var <- list(
#>         invari = c(                    # Control variables not changing over time
#>             "ADJ",                     # 两国是否相邻
#>             "LANG",                    # 两国是否有共同官方语言
#>             "Dist",                    # 两国地理距离（对数）
#>             "LEG_pretrans",            # 是否具有相同的法律体系
#>             "TrdInt"                   # 两国贸易强度（对数）
#>         ),
#>         vari = c(                      # Control variables changing over time
#>             "GDPSum",                  # 两国 GDP 合计（对数）
#>             "GDPSim",                  # 两国 GDP 相似度（对数）
#>             "PolStab",                 # 两国政治稳定性（对数）
#>             "IExpRisk",                # 两国逆向被征收风险（对数）
#>             "simi_skill_1559",         # 熟练劳动力份额相似度（对数）
#>             "diff_capShare_1559",      # 资本和熟练劳动力份额的绝对差异（对数）
#>             "diff_cap_low_1559",       # 两国资本和熟练劳动力之比的绝对差异（对数）
#>             "WTO"                      # 两国是否均为 WTO 成员
#>         )
#>     )
#>     data <- readRDS("/tmp/psm_test.Rds")
#>     treat <- "Q_MA"
#>     cov <- c(var$invari, var$vari)
#>     lag <- c(lapply(var$invari, function(x) 0L), lapply(var$vari, function(x) 1:3))
#>     match_result <- stxtpsm(data, treat, cov, lag, caliper = 0.001, std.caliper = FALSE)
#>     print(match_result)
#> })

