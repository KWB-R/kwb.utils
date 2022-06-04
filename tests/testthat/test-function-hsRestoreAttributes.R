test_that("hsRestoreAttributes() works", {
  
  x <- structure(1, a = 2, b = 3)
  
  y <- hsRestoreAttributes(c(0, x), attributes(x))

  expect_identical(attributes(y), attributes(x))
})
