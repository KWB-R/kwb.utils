test_that("readPackageFile() works", {
  
  expect_error(readPackageFile("no_such_file", "kwb.utils"))
  
  content <- readPackageFile(
    "dictionary.txt", "kwb.utils", sep = "=", header = FALSE, comment.char = "#"
  )
  
  expect_true(is.data.frame(content))
})
