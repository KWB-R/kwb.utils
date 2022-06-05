test_that("get_cached() works", {

  x <- 123
  
  kwb.utils:::cache_and_return(x)
  
  expect_output(expect_identical(kwb.utils:::get_cached("x"), x))
  expect_silent(expect_identical(kwb.utils:::get_cached("x", dbg = FALSE), x))
})
