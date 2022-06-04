test_that("generateKeyFile() works",  {
  
  x <- generateKeyFile()
  
  expect_true(is.character(x))
  
  expect_length(x, 27)
  
  target <- tempfile()
  
  x <- generateKeyFile(target)
  
  expect_identical(x, readLines(target))
})
