test_that("getFunctionValueOrDefault() works", {
  
  x <- 1:3
  
  expect_identical(getFunctionValueOrDefault(x, range), range(x))

  x <- c(NA, NA, NA)
  
  expect_warning(y <- getFunctionValueOrDefault(x, range, -1))

  expect_identical(y, -1)  
  
  expect_warning(getFunctionValueOrDefault(x, range, -1, "DEFAULT"))
})
