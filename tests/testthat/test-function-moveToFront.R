test_that("moveToFront() works", {
  
  y <- moveToFront(1:10, 5)
  
  expect_true(is.numeric(y))
  expect_length(y, 10)
  expect_identical(y[1], 5)
  
  x <- c("a", "b", "c", "x", "y", "d")
  y <- moveToFront(x, c("x", "y"))
  
  expect_true(is.character(y))
  expect_length(y, length(x))
  expect_identical(y[1:2], c("x", "y"))
})
