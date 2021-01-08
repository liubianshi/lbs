context("Test functions for pam+did analysise")

test_that("Test calculate propensity score for panel data", {
    data <- as.data.table(mtcars)
    data[, ID := rep(1:8, each = 4)] %>%
        .[, year := rep(2000:2003, 8)] %>%
        .[, treat := year >= (ID %% 4) + 2002]
    stxtset(data, "ID", "year")
    result <- stxtpsm(data, "treat", c("mpg", "cyl"), 1L)
    expect_equal(dim(result$data), c(22, 6))
    expect_equal(names(result$data)[5:6], c("pscore", "matchID"))
    expect_true(all(between(result$data$pscore, 0, 1)))
    expect_false(anyNA(result$data$pscore))
    expect_equal(anyDuplicated(result$data[1:2]), 0L)

    result2 <- stxtpsm(data, "treat", c("mpg", "cyl"), lag= list(mpg = c(0,1), cyl = 1),
                       method = "probit")
    expect_equal(dim(result2$data), c(22, 6))
    expect_equal(dim(result2$check$balance), c(4, 6))
})



