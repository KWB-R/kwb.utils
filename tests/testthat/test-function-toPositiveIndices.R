test_that("toPositiveIndices() works", {
  
  expect_identical(toPositiveIndices(c(-1, -2), n = 10), c(10, 9))
  expect_identical(toPositiveIndices(c(1, -1), n = 10), c(1, 10))
})
