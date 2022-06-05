test_that("clipMatrix() works", {

  f <- kwb.utils:::clipMatrix

  expect_error(f())
  m <- matrix(ncol = 3L, byrow = TRUE, c(
    NA, NA, NA,
    NA, 1, 2, 
    NA, 3, 4,
    NA, NA, NA,
    NA, NA, NA
  ))

  m2 <- f(m)
  
  expect_identical(m2, m[-c(1L, 4L, 5L), -1L])
  expect_identical(f(m2), m2)
})
