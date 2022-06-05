test_that("toConditional() works", {

  f <- kwb.utils:::toConditional

  expect_error(f())

  result <- f(function(x) print("hello"))
  
  expect_is(result, "function")
  
  expect_output(result(NULL, TRUE))
  expect_silent(result(NULL, FALSE))
})
