test_that("almostEqual() works", {
  
  expect_error(almostEqual(1, 1:2))
  expect_error(almostEqual(1:2, 3))
  
  expect_false(almostEqual(1, 2))
  expect_true(almostEqual(1, 1))
  
  expect_false(almostEqual(1, 1.1, 0.1))
  expect_true(almostEqual(1, 1.09, 0.1))
})
