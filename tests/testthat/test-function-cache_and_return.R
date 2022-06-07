test_that("cache_and_return() works", {

  f <- kwb.utils:::cache_and_return
  
  expect_error(f())

  x <- 1
  
  expect_identical(f(x), x)
  expect_true(file.exists(kwb.utils:::get_cached_file("x")))
})
