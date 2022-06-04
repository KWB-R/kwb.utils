test_that("allAreEqual() works", {
  
  x_1 <- c(1, 1, 1)
  x_2 <- c(1, 1, 2)
  
  expect_true(allAreEqual(x_1))
  expect_false(allAreEqual(x_2))
  
  expect_true(allAreEqual(x_1, method = 2))
  expect_false(allAreEqual(x_2, method = 2))
})
