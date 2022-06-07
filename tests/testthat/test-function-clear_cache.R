test_that("clear_cache() works", {

  f <- kwb.utils:::clear_cache
  
  expect_output(f())
  expect_output(f(), "No cached files")
  
  x <- 1
  kwb.utils:::cache_and_return(x)
  
  expect_output(f(), "Clearing")
  
  expect_null(kwb.utils:::get_cached("x"))
})
