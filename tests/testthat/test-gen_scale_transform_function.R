context("Test gen scale transform function")

test_that("Test gen_scale_transform_function", {
    transcale <- gen_scale_transform_function(c(1,10), c(500, 1000))
    expect_equal(transcale$forword(1), 500)
    expect_equal(transcale$backword(1000), 10)
    expect_equal(transcale$backword(transcale$forword(5)), 5)
})
