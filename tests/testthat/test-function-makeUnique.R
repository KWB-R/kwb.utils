test_that("makeUnique() works", {

  x <- c("a", "a")
  
  expect_warning(y <- makeUnique(x))
  
  expect_true(! any(duplicated(y)))
  
  expect_silent(makeUnique(x, warn = FALSE))
})
