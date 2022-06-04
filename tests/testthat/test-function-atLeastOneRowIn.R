test_that("atLeastOneRowIn() works", {
  
  expect_true(atLeastOneRowIn(data.frame(a = 1)))
  expect_true(atLeastOneRowIn(matrix(1)))
  
  expect_false(atLeastOneRowIn(data.frame(a = character())))
})
