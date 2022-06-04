test_that("percentageOfMaximum() works", {

  values <- 1:10
  
  expect_equal(percentageOfMaximum(values) / 100 * max(values), values)
  
  expect_true(all(is.na(percentageOfMaximum(c(1:3, NA), na.rm = FALSE))))
})
