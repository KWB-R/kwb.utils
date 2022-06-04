test_that("defaultIfNULL(), defaultIfNA(), defaultIfZero() work", {
  
  expect_identical(defaultIfNULL(NULL, "default"), "default")
  expect_identical(defaultIfNA(NA, "default"), "default")
  
  expect_identical(defaultIfNULL("actual", "default"), "actual")
  expect_identical(defaultIfNA("actual", "default"), "actual")
  
  expect_length(which(is.na(defaultIfZero(c(1, 2, 0, 4, 5, 0, 6), NA))), 2L)
  
  y <- defaultIfZero(1:6, NA, count = TRUE)
  expect_identical(attr(y, "count"), 0L)
})
