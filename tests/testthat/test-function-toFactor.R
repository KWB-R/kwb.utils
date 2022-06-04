test_that("toFactor() works", {
  
  x <- c("b", "c", "a")
  
  y <- toFactor(x)
  
  expect_identical(levels(y), x)
  
  expect_identical(toFactor(y), y)
})
