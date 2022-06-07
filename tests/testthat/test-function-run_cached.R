test_that("run_cached() works", {

  f <- kwb.utils:::run_cached
  
  expect_error(f())

  expect_output(kwb.utils:::clear_cache())
  
  expect_silent(result <- f("abc", {x <- 1; y <- 2 * x}))
  expect_identical(result, 2)

  expect_output(result <- f("abc", {x <- 1; y <- 2 * x}))
  expect_identical(result, 2)
  
  expect_output(result <- kwb.utils:::get_cached("abc"))
  expect_identical(result, 2)
})
