test_that("getOddNumbers() and getEvenNumbers() works", {
  
  expect_equal(getOddNumbers(1:10), c(1, 3, 5, 7, 9))
  expect_equal(getEvenNumbers(1:10), c(2, 4, 6, 8, 10))
})
