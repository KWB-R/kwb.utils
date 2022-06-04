test_that("safeColumnBind() works", {

  x <- data.frame(a = 1:3, b = rnorm(3))

  expect_identical(safeColumnBind(NULL, x), x)
  
  expect_identical(safeColumnBind(x, x), cbind(x, x))
})
