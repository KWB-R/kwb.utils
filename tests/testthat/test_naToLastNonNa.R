test_that("naToLastNonNa works as expected", {
  
  expect_identical(
    naToLastNonNa(c(1, NA, 2, NA)), 
    c(1, 1, 2, 2)
  )

  expect_error(
    naToLastNonNa(c(NA, 1, NA), method = 1)
  )

  expect_identical(
    naToLastNonNa(c(NA, 1, NA), method = 2),
    c(NA, 1, 1)
  )
  
  expect_identical(
    naToLastNonNa(c(NA, NA)),
    c(NA, NA)
  )
})
