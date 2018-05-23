test_that("getFunctionValueOrDefault() works", {
  
  x <- 1:3
  
  expect_identical(getFunctionValueOrDefault(x, range), range(x))

  x <- c(NA, NA, NA)
  
  expect_warning(y <- getFunctionValueOrDefault(x, range, -1))

  expect_identical(y, -1)  
  
  expect_warning(getFunctionValueOrDefault(x, range, -1, "DEFAULT"))
})

test_that("defaultLevels() works", {
  
  expect_equal(defaultLevels(c(1, 3, 4, 5, 4)), 1:5)
  
  expect_equal(
    defaultLevels(c(1920, 1950, 1970), step = 10), seq(1920, 1970, 10)
  )
})

test_that("defaultIfNULL(), defaultIfNA(), defaultIfZero() work", {
  
  expect_identical(defaultIfNULL(NULL, "default"), "default")
  expect_identical(defaultIfNA(NA, "default"), "default")
  
  expect_identical(defaultIfNULL("actual", "default"), "actual")
  expect_identical(defaultIfNA("actual", "default"), "actual")
  
  expect_length(which(is.na(defaultIfZero(c(1, 2, 0, 4, 5, 0, 6), NA))), 2L)
  
  y <- defaultIfZero(1:6, NA, count = TRUE)
  expect_identical(attr(y, "count"), 0L)
})

# .defaultIf -------------------------------------------------------------------
