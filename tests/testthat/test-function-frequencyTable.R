test_that("frequencyTable() works", {
  
  data <- data.frame(
    A = c("a1", "a2", "a1", "a1", "a2", "", "a2", NA, "a1"),
    B = c("b1", "b1", NA, "b2", "b2", "b1", " ", "b3", "b2")
  )

  y_df <- frequencyTable(data)
  y_list <- frequencyTable(data, as.data.frame = FALSE)
  
  expect_true(is.data.frame(y_df))
  expect_true(is.list(y_list))
  
  elements <- c("column", "value", "count")
  
  expect_identical(names(y_df), elements)
  expect_identical(names(y_list), names(data))
  
  for (y in y_list) {
    
    expect_identical(names(y), elements)
  }
})
