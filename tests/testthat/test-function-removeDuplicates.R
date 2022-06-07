test_that("removeDuplicates() works", {

  f <- kwb.utils:::removeDuplicates

  expect_error(f(dbg = FALSE))
  
  expect_output(result <- f(c(1, 1), dbg = TRUE), "Removing 1")
  expect_identical(result, 1)

  expect_output(result <- f(1:5))
  expect_identical(result, 1:5)
})
