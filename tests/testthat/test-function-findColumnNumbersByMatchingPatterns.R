test_that("findColumnNumbersByMatchingPatterns() works", {

  f <- kwb.utils:::findColumnNumbersByMatchingPatterns

  expect_error(f())

  columnDescription <- list(
    x = list(match = "^x", colNumber = integer()),
    y = list(match = "^y", colNumber = 2)
  )
  
  y <- f(headerFields = c("x1", "x2", "y1", "y2"), columnDescription)
  
  expect_type(y, "list")
  expect_length(y, length(columnDescription))
  
  col_number_available <- sapply(y, function(xx) ! is.null(xx$colNumber))
  
  expect_true(all(col_number_available))
})
