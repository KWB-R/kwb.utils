test_that("hsMovingMean() work", {

  x <- 1:10
  
  expect_error(hsMovingMean(x, n = 2))
  
  y_1 <- hsMovingMean(x, n = 1)
  y_3 <- hsMovingMean(x, n = 3)
  y_5 <- hsMovingMean(x, n = 5)
  
  expect_equal(y_1, x)
  expect_identical(sum(is.na(y_3)), 2L)
  expect_identical(sum(is.na(y_5)), 4L)
})
