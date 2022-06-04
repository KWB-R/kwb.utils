test_that(".defaultIf() works with a vector of length > 1", {
  expect_equal(.defaultIf(is.na, c(1, NA, 3), 2), 1:3)
  expect_equal(.defaultIf(isOddNumber, 1:5, 77), c(77, 2, 77, 4, 77))
})
