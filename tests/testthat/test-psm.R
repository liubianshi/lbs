context("Test functions for pam+did analysise")

test_that("Test calculate propensity score for panel data", {
    data <- as.data.table(mtcars)
    data[, ID := rep(1:8, each = 4)] %>%
        .[, year := rep(2000:2003, 8)] %>%
        .[, treat := year >= (ID %% 4) + 2002]
    stxtset(data, "ID", "year")
    result <- calpscore_panel(data, "treat", c("mpg", "cyl"), 1L)
    expect_equal(dim(result), c(22, 4))
    expect_equal(names(result), c("ID", "year", "pscore"))
    expect_true(all(between(result$pscore, 0, 1)))
    expect_false(anyNA(result$pscore))
    expect_equal(anyDuplicated(result[1:2]), 0L)
    print(result)

    result2 <- calpscore_panel(data, "treat", c("mpg", "cyl"), method = "probit")
    expect_equal(dim(result2), c(30, 4))
})



