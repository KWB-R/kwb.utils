context("Lookup-functions")

keys <- c("A", "B", "C")
values <- c("Apple", "Banana", "Cherry")

test_that("toLookupClass() works", {

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
})

test_that("toLookupList() works", {
  
  y <- toLookupList(keys, values)
  
  expect_true(is.list(y))
  expect_length(y, length(keys))
  expect_identical(names(y), keys)
  expect_identical(as.character(y), values)
})

test_that("toLookupTable() works", {

  y <- toLookupTable(keys, values)
  
  expect_true(is.data.frame(y))
  expect_identical(nrow(y), 1L)
  expect_identical(names(y), keys)
  expect_identical(as.character(y), values)
})

test_that("tableLookup() works", {
  
  x <- toLookupTable(keys, values, as.twoColumnTable = TRUE)
  
  for (i in seq_along(keys)) {
  
    expect_identical(tableLookup(x, keys[i]), values[i])
  }
})
