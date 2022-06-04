test_that("getByPositiveOrNegativeIndex() works", {

  f <- getByPositiveOrNegativeIndex
  x <- 1:10
  
  expect_error(f(x, 11))
  
  expect_equal(f(x, -1), tail(x, 1))
  expect_equal(f(x,  1), head(x, 1))
})
