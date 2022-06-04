test_that("addRowWithName() works", {

  x <- data.frame(a = 1:2, b = 3:4)
  
  expect_error(addRowWithName(x, data.frame(a = 5:6, b = 7:8)))
  expect_error(addRowWithName(x, data.frame(a = 10)))
               
  y <- addRowWithName(x, data.frame(a = 1 + 2, b = 3 + 4), "sum")
  
  expect_identical("sum", lastElement(rownames(y)))
})
