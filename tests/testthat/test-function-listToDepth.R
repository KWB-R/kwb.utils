#library(testthat)
test_that("listToDepth() works", {

  f <- kwb.utils:::listToDepth

  expect_message(expect_error(f()))
})
