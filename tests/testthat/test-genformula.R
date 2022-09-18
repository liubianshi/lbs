context("Test Formula Generation")

test_that("Tess Linear formula generation", {
    eq <- as.formula("a ~ b + c")
    eq_fixest <- as.formula("y ~ x1 | fe1 + fe2 | x2 ~ z1 + z2")
    expect_equal(genformula(c("a", "b", "c"), method = "linear"), eq)
    expect_equal(genformula(c("y", "x1"),
                            fe = c("fe1", "fe2"),
                            en = "x2",
                            iv = c("z1", "z2"),
                            method = "fixest"), eq_fixest)
})
