context("Test number format")

test_that("Test stformat", {
    numlist <- 1.23456 * 10^(-5:5)
    chalist <- c(
        "0.000",  "0.000",   "0.001",
        "0.012",  "0.123",   "1.235",
        "12.35",  "123.5",   " 1,235",
        "12,346",  "123,456"
    )
    expect_equal(stformat(numlist, width = 5), chalist)
    expect_equal(stformat(2L, width = 6), "     2")
    expect_equal(stformat(0, digits = 2, width = 5), " 0.00")
})
