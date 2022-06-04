test_that("extendLimits() works", {

  expect_identical(extendLimits(c(-1, 1), 1, 1), c(-3, 3))
  expect_identical(extendLimits(c(-1, 1), 1, 1, absolute = TRUE), c(-2, 2))
})
