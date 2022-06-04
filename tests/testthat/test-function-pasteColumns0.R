test_that("pasteColumns0() and pasteColumns() work", {

  x <- data.frame(A = 1:3, B = 2:4)
  
  y_1 <- pasteColumns0(x)
  y_2 <- pasteColumns(x, sep = ";")
  
  expect_is(y_1, "character")
  expect_is(y_2, "character")
  
  expect_identical(y_1, paste0(x$A, x$B))
  expect_identical(y_2, paste(x$A, x$B, sep = ";"))
  
  expect_error(pasteColumns0(x, "C"))
  expect_error(pasteColumns(x, "C"))
})
