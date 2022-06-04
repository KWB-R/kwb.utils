test_that("colStatistics() works", {

  x <- data.frame(a = 1:10, b = 2:11)
  y <- colStatistics(x)
  
  expect_identical(dim(y), c(2L, 6L))
  
  expect_equal(y$sum, c(sum(x$a), sum(x$b)))
  expect_equal(y$min, c(min(x$a), min(x$b)))
  expect_equal(y$max, c(max(x$a), max(x$b)))
  
  y <- colStatistics(x, functionColumn = TRUE)
  
  expect_true(! is.null(y$FUN))
  
  expect_error(colStatistics(x, functions = "Min"))
})
