#library(testthat)
test_that("listFiles() works", {

  f <- kwb.utils:::listFiles

  expect_message(expect_is(f(), "data.frame"))
  expect_silent(f(silent = TRUE))
  expect_silent(f(path = character(), silent = TRUE))
  
})
