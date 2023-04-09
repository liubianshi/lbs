context("Test functions for did analysise")

sample_for_test <- gen_example_data_for_did(10, 200, 7)
re <- event_study_result(sample_for_test, "id", "time", "y",
                   never_treat = Inf,
                   group = "first_treat",
                   covs = c("cov_1", "cov_2"))

test_that("Test parallel trent", {
    expect_equal(names(re), c("var", "exposure", "coef", "se"))
})


