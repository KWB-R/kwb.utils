test_that("randomValuesWithSum() works", {
  
  y <- randomValuesWithSum(10, 100)
  
  expect_equal(sum(y), 100)
  expect_named(y)
  expect_identical(names(y), as.character(seq_len(10)))
})
