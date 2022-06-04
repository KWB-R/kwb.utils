test_that("recycle() works", {

  x <- 1:10
  y <- recycle(x, 20)
  
  expect_equal(recycle(x), x)
  expect_length(y, 20)
  expect_equal(y[1:10], y[11:20])
})
