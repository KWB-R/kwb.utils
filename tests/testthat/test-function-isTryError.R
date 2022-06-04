test_that("is_try_error() works", {

  expect_error(isTryError())
  expect_false(isTryError(try(x <- 1)))
  
  expect_true(isTryError(
    try(read.table("no-such-file.txt"), silent = TRUE)
  ))
  
})
