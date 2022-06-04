test_that("naToLastNonNa works as expected", {
  
  expect_identical(naToLastNonNa(c(1, NA, 2, NA)), c(1, 1, 2, 2))

  expect_error(naToLastNonNa(c(NA, 1, NA, 2), method = 1))
  
  expect_identical(naToLastNonNa(c(1, 1, NA, 2), method = 1), c(1, 1, 1, 2))
  
  expect_identical(naToLastNonNa(c(NA, 1, NA, 2)), c(NA, 1, 1, 2))
  
  expect_identical(naToLastNonNa(c(NA, NA)), c(NA, NA))
  
  expect_identical(
    naToLastNonNa(c(1, 2, NA, NA, 3, NA, NA, 4, NA, NA, 5)),
    c(1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5)
  )
})
