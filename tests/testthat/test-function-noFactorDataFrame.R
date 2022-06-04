test_that("noFactorDataFrame() works", {

  f <- kwb.utils:::noFactorDataFrame

  result <- f()

  expect_is(result, "data.frame")
  
  result <- f(a = "hello")
  
  expect_is(result$a, "character")  
})
