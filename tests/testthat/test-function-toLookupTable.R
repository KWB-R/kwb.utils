test_that("toLookupTable() works", {

  keys <- c("A", "B", "C")
  values <- c("Apple", "Banana", "Cherry")
  
  y <- toLookupTable(keys, values)
  
  expect_true(is.data.frame(y))
  expect_identical(nrow(y), 1L)
  expect_identical(names(y), keys)
  expect_identical(as.character(y), values)
  
  List <- list(a = 1, b = 2)
  y <- toLookupTable(List = List)
  
  expect_identical(names(y), names(List))
  expect_identical(as.integer(y), as.integer(List))
  
  expect_error(toLookupTable(List = 1))
})
