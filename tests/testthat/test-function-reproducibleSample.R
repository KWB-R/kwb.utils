test_that("reproducibleSample() works", {

  f <- kwb.utils:::reproducibleSample

  expect_error(f())

  result1 <- f(10)
  
  str(result1)
  
  seed <- attr(result1, "random_seed")
  
  expect_true(!is.null(seed))
  
  result2 <- f(10, random_seed = seed)
  
  expect_identical(result1, result2)
})
