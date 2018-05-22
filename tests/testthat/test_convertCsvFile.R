test_that("convertCsvFile() works", {
  
  csv_in <- tempfile(fileext = ".csv")
  
  write.table(iris, csv_in, row.names = FALSE)

  csv_out <- convertCsvFile(csv_in, sep_out = ";")

  on.exit(unlink(csv_out))
  
  expect_true(is.character(csv_out))
  expect_length(csv_out, 1)
  
  iris_reread <- read.table(csv_out, sep = ";", header = TRUE)
  
  expect_identical(iris, iris_reread)

  unlink(csv_out)
  
  csv_out <- convertCsvFile(csv_in, sep_out = ";", dec_out = ",")

  expect_true(is.character(csv_out))
  expect_length(csv_out, 1)

  iris_reread <- read.table(csv_out, sep = ";", dec = ",", header = TRUE)
  
  expect_identical(iris, iris_reread)
})
