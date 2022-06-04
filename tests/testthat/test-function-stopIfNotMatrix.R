test_that("stopIfNotMatrix() works", {
  
  expect_error(stopIfNotMatrix(NULL))
  expect_error(stopIfNotMatrix(data.frame()))
  expect_silent(stopIfNotMatrix(matrix()))
})
