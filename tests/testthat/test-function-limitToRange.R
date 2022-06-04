test_that("limitToRange() works", {
  
  expect_identical(range(limitToRange(1:20, left = 5, right = 15)), c(5, 15))
  expect_identical(range(limitToRange(1:20, left = -10, right = 50)), c(1, 20))
  
  expect_error(limitToRange(1:10, left = 1:2, right = 5))
})
