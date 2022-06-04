test_that("hsStringToDouble() works", {
  
  expect_identical(hsStringToDouble("1.1"), 1.1)
  expect_warning(hsStringToDouble("1,1"))
  expect_identical(hsStringToDouble("1,1", dec = ","), 1.1)
  expect_error(hsStringToDouble("1.1.1"))
  
  expect_error(hsStringToDouble("1.1", dec = "*"))
  
  expect_warning(hsStringToDouble("1.1", dec = ","))
})
