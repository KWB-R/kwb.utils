test_that("asColumnList() and asRowList() work", {
  
  x <- matrix(1:12, nrow = 3)

  column_list <- asColumnList(x)
  row_list <- asRowList(x)
  
  expect_is(column_list, "list")
  expect_is(row_list, "list")
  
  expect_length(column_list, ncol(x))
  expect_length(row_list, nrow(x))
  
  for (i in 1:ncol(x)) {
    
    expect_identical(column_list[[i]], x[, i])
  }
  
  for (i in 1:nrow(x)) {
    
    expect_identical(row_list[[i]], x[i, ])
  }
})
