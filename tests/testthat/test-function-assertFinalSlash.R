test_that("assertFinalSlash() works", {

  f <- kwb.utils::assertFinalSlash

  expect_error(f())

  x <- c("a", "b/")
  
  results <- lapply(1:3, f, x = x)
  
  expect_true(all(sapply(results, function(x) {
    all(hasFinalSlash(x))
  })))
})
