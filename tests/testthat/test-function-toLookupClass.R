test_that("toLookupClass() works", {
  keys <- c("A", "B", "C")
  values <- c("Apple", "Banana", "Cherry")
  
  fruits_df1 <- toLookupClass(keys, values)
  fruits_df2 <- toLookupClass(keys, values, class = "data.frame.2")
  fruits_list <- toLookupClass(keys, values, class = "list")
  fruits_vector <- toLookupClass(keys, values, class = "vector")
  
  for (i in seq_along(keys)) {
    expect_identical(fruits_df1[[keys[i]]], values[i])
    expect_identical(fruits_list[[keys[i]]], values[i])
  }
  
  expect_identical(fruits_vector["A"], c(A = "Apple"))
  
  expect_identical(
    fruits_df1[c("A", "C")], 
    data.frame(A = "Apple", C = "Cherry", stringsAsFactors = FALSE)
  )
               
  expect_identical(fruits_list[c("A", "C")], list(A = "Apple", C = "Cherry"))
  expect_identical(fruits_vector[c("A", "C")], c(A = "Apple", C = "Cherry"))
  expect_error(toLookupClass(keys, values, class = "no_such_class"))
})
