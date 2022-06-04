test_that("percentageOfSum() works", {

  expect_identical(sum(percentageOfSum(1:10)), 100)
  
  expect_true(all(is.na(percentageOfSum(c(1:3, NA), na.rm = FALSE))))
})
