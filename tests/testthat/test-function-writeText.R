test_that("writeText() works", {
  
  file <- tempfile()
  
  x <- c("Hello", "world")
  
  expect_output(file_returned <- writeText(x, file))
  
  expect_identical(file_returned, file)
  
  type <- "welcome file"
  
  expect_output(writeText(x, file, type = type), regexp = type)
  
  expect_identical(x, readLines(file))
})
