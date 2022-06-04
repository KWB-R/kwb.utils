test_that("fileExtension() works", {
  
  paths <- c("C:/example/file.csv", "file2.txt", "D:/e/f/ghi.jkl.zip")
  
  expect_equal(fileExtension(paths), c("csv", "txt", "zip"))
  expect_equal(fileExtension("C:/NEWS"), "")
})
