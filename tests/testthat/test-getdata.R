context("Test get Data")

test_that("Test getDataSQLite", {
    vars <- c("oriID", "year", "F143", "F167")
    result <- getDataSQLite( "CHN_IndsFirm", "Stat2011", vars, limit = 10)
    expect_true(all.equal(dim(result), c(10, 4)))
    expect_equal(names(result), vars)
    expect_equal(attr(result$F143, "label"), "主营收入")
    expect_equal(attr(result$oriID, "label"), "记录在原始数据中的编号")
})
