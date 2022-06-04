test_that("mergeAll() and safeMerge() work", {
  
  to_data <- function(...) data.frame(stringsAsFactors = FALSE, ...)
  
  peter <- to_data(fruit = c("apple", "pear", "banana"), kg = 1:3)
  paul <- to_data(fruit = c("banana", "apple", "lemon"), kg = c(10, 20, 30))
  mary <- to_data(fruit = c("lemon", "organger", "apple"), kg = c(22, 33, 44))

  expect_error(safeMerge(peter, paul, by = "no_such_column"))
  expect_silent(y <- safeMerge(peter, paul, by = "fruit"))
  
  expect_identical(y, merge(peter, paul, by = "fruit"))
  
  x_1 <- list(peter = peter, paul = paul, mary = mary)
  x_2 <- list(peter, paul, mary)
  
  expect_output(y_1a <- mergeAll(x_1, by = "fruit"))
  expect_output(y_1b <- mergeAll(x_1, by = "fruit", all = TRUE))
  expect_output(y_2b <- mergeAll(x_2, by = "fruit", all = TRUE))
  
  expect_silent(mergeAll(x_1, by = "fruit", dbg = FALSE))

  expect_identical(nrow(y_1a), 1L)
  
  expect_identical(
    sort(y_1b$fruit), 
    sort(unique(c(peter$fruit, paul$fruit, mary$fruit)))
  )
  
  expect_identical(unname(y_1b), unname(y_2b))
})
