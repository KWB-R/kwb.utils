test_that("roundColumns() works", {
  
  f <- roundColumns
  
  x <- data.frame(
    a = c(1.23456, 234.5678), 
    b = c("A", "B"), 
    c = c(1245, 234)
  )
  
  expect_warning(
    f(x, columnNames = "a", digits = 2), 
    "'columnNames' is deprecated"
  )
  
  x1 <- f(x, columns = c("a", "c"), digits = 2)
  x2 <- f(x, digits = list(a = 1, c = -2))
  x3 <- f(x, digits = 1, pattern = "a")

  expect_equal(x1$a, round(x$a, 2))
  expect_equal(x1$c, round(x$c, 2))
  
  expect_equal(x2$a, round(x$a, 1))
  expect_equal(x2$c, round(x$c, -2))
  
  expect_equal(x3$a, round(x$a, 1))
  expect_identical(x3$c, x$c) # column c was not touched
  
  expect_identical(f(x, columns = NULL), x)
})
