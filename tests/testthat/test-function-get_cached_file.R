test_that("get_cached_file() works", {

  f <- kwb.utils:::get_cached_file
  
  expect_output(kwb.utils:::clear_cache())
  expect_length(f(), 0L)
  expect_identical(basename(f("x")), "x.RData")
})
