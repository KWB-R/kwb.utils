test_that("setMatrixColumns() works", {

  x <- createMatrix(c("a", "b", "c"))  
  
  y <- setMatrixColumns(x, list(c = 1:3))

  expect_warning(setMatrixColumns(x, list(r = 1)))
  
  expect_equal(unname(y[, "c"]), 1:3)
})
