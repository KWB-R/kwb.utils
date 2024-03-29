test_that("convertCsvFile() works", {
  
  csv_in_1 <- tempfile(fileext = ".csv")
  csv_in_2 <- tempfile(fileext = ".csv")
  csv_in_3 <- tempfile(fileext = ".csv")
  
  write.table(iris, csv_in_1, row.names = FALSE)
  write.table(iris, csv_in_2, row.names = TRUE)
  write.table(iris, csv_in_3, col.names = FALSE)
  
  capture.output(csv_out <- convertCsvFile(csv_in_1, row.names_in = rownames(iris)))
  unlink(csv_out)
  
  capture.output(csv_out <- convertCsvFile(csv_in_1, col.names_in = colnames(iris)))
  unlink(csv_out)
  
  check_result <- function(csv_in, args_convert, args_read) {
  
    csv_out <- do.call(convertCsvFile, c(list(csv_in, dbg = FALSE), args_convert))
    
    on.exit(unlink(csv_out))
  
    # Test that existing file will not be overwritten
    expect_error(convertCsvFile(csv_in, dbg = FALSE))
    expect_true(is.character(csv_out))
    expect_length(csv_out, 1)
    expect_identical(iris, do.call(read.table, c(list(csv_out), args_read)))
  }

  # The default for stringsAsFactors seems to have changed in some recent 
  # version of R!
  
  check_result(
    csv_in = csv_in_1, 
    args_convert = list(sep_out = ";"), 
    args_read = list(sep = ";", header = TRUE, stringsAsFactors = TRUE)
  )
  
  check_result(
    csv_in = csv_in_1, 
    args_convert = list(sep_out = ";", dec_out = ","),
    args_read = list(
      sep = ";", dec = ",", header = TRUE, stringsAsFactors = TRUE
    )
  )
})
