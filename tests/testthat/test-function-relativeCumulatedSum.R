test_that("relativeCumulatedSum() works", {

  expect_identical(relativeCumulatedSum(1), 100)
  
  expect_identical(lastElement(relativeCumulatedSum(1:10)), 100)
  expect_identical(order(relativeCumulatedSum(1:5)), 1:5)
})
