test_that("allAreIdentical() works", {
  
  expect_error(allAreIdentical(1))
  expect_error(allAreIdentical(list()))
  
  expect_message(allAreIdentical(list(1)))
  
  expect_true(allAreIdentical(list(1, 1)))
  expect_false(allAreIdentical(list(1, 2)))
})
