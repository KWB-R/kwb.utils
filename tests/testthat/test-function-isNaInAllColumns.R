test_that("isNaInAllColumns() and isNaInAllRows() work", {
  
  x_1 <- data.frame(a = 1:2, b = NA)
  x_2 <- rbind(x_1, data.frame(a = NA, b = NA))
  
  expect_identical(isNaInAllColumns(x_1), c(FALSE, FALSE))
  expect_identical(isNaInAllColumns(x_2), c(FALSE, FALSE, TRUE))
  
  expect_identical(isNaInAllRows(x_1), c(FALSE, TRUE))
  expect_identical(isNaInAllRows(x_2), c(FALSE, TRUE))
  
  expect_error(isNaInAllColumns(1))
  expect_error(isNaInAllRows(1))
})
