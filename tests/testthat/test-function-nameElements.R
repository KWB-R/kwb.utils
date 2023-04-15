#library(testthat)
test_that("nameElements() works", {

  f <- kwb.utils:::nameElements

  expect_error(f())
  
  x <- 1:3
  expect_identical(names(f(x)), c("x1", "x2", "x3"))
  expect_identical(names(f(x, prefix = "y")), c("y1", "y2", "y3"))
  expect_identical(names(f(x, defaults = c("a", "b", "c"))), c("a", "b", "c"))

  x <- c(a = 1, b = 2)
  expect_identical(f(x), x)
  expect_identical(f(x, defaults = c("does", "not", "matter")), x)
  
  x <- c(a = 1, 2)
  expect_identical(names(f(x)), c("a", "x2"))
  expect_identical(names(f(x, defaults = c("A", "B"))), c("a", "B"))
  expect_identical(names(f(x, prefix = "p.")), c("a", "p.2"))
})
