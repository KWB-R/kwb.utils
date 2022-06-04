#library(testthat)
#library(kwb.utils)

test_that("catChangesIf() works", {

  f <- catChangesIf

  expect_error(f())
  expect_output(f(TRUE, 1:2, 2:3))
})
