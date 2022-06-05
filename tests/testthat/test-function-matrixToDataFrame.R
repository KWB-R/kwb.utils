test_that("matrixToDataFrame() works", {

  f <- kwb.utils:::matrixToDataFrame

  expect_error(f())
  expect_error(f(1), "is not a matrix")
  
  m <- matrix(1:4, nrow = 2)
  
  result1 <- f(m)
  
  expect_is(result1, "data.frame")
  expect_identical(result1$value, as.integer(t(m)))
})
