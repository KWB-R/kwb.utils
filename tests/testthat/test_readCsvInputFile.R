text_file <- extdataFile("example-1.csv")

test_that("columnDescriptor() works", {
  
  y <- columnDescriptor()
  
  expect_is(y, "list")
  expect_identical(names(y), c("match", "fixed"))
})

columnDescription_1 <- list(
  columnDescriptor("a"),
  columnDescriptor("b"),
  columnDescriptor("c"),
  columnDescriptor("d")
)

columnDescription_2 <- list(
  z = columnDescriptor("z")
)

columnDescription_3 <- list(
  columnDescriptor("a"),
  columnDescriptor("^a")
)

test_that("readCsvInputFile() works", {

  expect_error(readCsvInputFile("no_such_file"))
  
  expect_error(readCsvInputFile(text_file, headerPattern = "no_such_header"))

  expect_error(readCsvInputFile(
    text_file, sep = ",", dec = ".", columnDescription = columnDescription_1
  ))
  
  expect_error(readCsvInputFile(
    text_file, sep = ",", dec = ".", columnDescription = columnDescription_2
  ))
  
  y <- readCsvInputFile(text_file, ";", ".")
  
  expect_is(y, "data.frame")
  
  expect_warning(
    readCsvInputFile(text_file, sep = ",", columnDescription = list())
  )
})

test_that("msgAvailableFields() works", {
  
  expect_is(msgAvailableFields(1:3), "character")
})

test_that("defaultColumnDescription() works", {
  
  headerFields <- c("a", "b", "c")
  
  y <- defaultColumnDescription(headerFields)

  expect_is(y, "list")
  
  expect_identical(names(y), headerFields)
})
