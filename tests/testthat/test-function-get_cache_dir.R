test_that("get_cache_dir() works", {

  f <- kwb.utils:::get_cache_dir

  result <- f()

  expect_is(result, "character")
  expect_length(result, 1L)
})
