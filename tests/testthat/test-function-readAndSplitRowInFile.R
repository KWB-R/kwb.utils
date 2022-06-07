#library(testthat)
test_that("readAndSplitRowInFile() works", {

  f <- kwb.utils:::readAndSplitRowInFile

  expect_error(f())

  text_file <- safePath(extdataFile("example-1.csv"))
  
  arguments <- list(text_file, rowNumber = 1, sep = ",", encoding = "UTF-8")
  
  y_1 <- do.call(f, c(arguments, version = 1))
  y_2 <- do.call(f, c(arguments, version = 2))
  
  header_fields <- c("a", "b", "c")
  
  expect_identical(y_1, header_fields)
  expect_identical(y_2, header_fields)
  
})
