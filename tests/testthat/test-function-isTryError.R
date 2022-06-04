test_that("isTryError() works", {

  f <- isTryError
  
  expect_error(f())
  expect_false(f(try(x <- 1)))

  suppressWarnings(result <- try(read.table("no-such-file.txt"), silent = TRUE))
  
  expect_true(f(result))
  
})
