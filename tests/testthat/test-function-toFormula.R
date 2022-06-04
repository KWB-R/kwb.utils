test_that("toFormula() works", {

  f1 <- toFormula("y", c("x1", "x2"))
  f2 <- toFormula("y", paste0("x", 1:20))
  f3 <- toFormula("BMI", c("height", "mass"), as.formula = FALSE)

  expect_s3_class(f1, "formula")
  expect_s3_class(f2, "formula")
  expect_true(is.character(f3))
})
