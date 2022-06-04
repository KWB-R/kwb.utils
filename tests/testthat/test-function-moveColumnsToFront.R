test_that("moveColumnsToFront() works", {
  
  df <- data.frame(a = 1:5, b = 2:6, c = 3:7)
  df_one_column <- data.frame(a = 1:4)
  
  y_1 <- moveColumnsToFront(df, "b")
  y_2 <- moveColumnsToFront(df, c("b", "a"))
  y_3 <- moveColumnsToFront(df)

  expect_identical(dim(df), dim(y_1))
  expect_identical(dim(df), dim(y_2))  

  expect_identical(names(y_1)[1], "b")
  expect_identical(names(y_2)[1:2], c("b", "a"))
  
  expect_identical(y_3, df)
  
  expect_identical(moveColumnsToFront(df_one_column, "a"), df_one_column)
})
