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

test_that(".stopIfNotEnoughColumns() works", {
  
  headerFields <- c("a", "b", "c")
  
  expect_error(.stopIfNotEnoughColumns(headerFields, columnDescription_1, ","))
  expect_silent(.stopIfNotEnoughColumns(headerFields, columnDescription_2, ","))
})

test_that(".numberedEnumeration() works", {
  
  y <- .numberedEnumeration(LETTERS[1:5])
  
  expect_is(y, "character")
})

test_that(".findColumnNumbersByMatchingPatterns() works", {

  columnDescription <- list(
    x = list(match = "^x", colNumber = integer()),
    y = list(match = "^y", colNumber = 2)
  )

  y <- .findColumnNumbersByMatchingPatterns(
    headerFields = c("x1", "x2", "y1", "y2"), 
    columnDescription
  )
  
  expect_is(y, "list")
  expect_length(y, length(columnDescription))
  
  col_number_available <- sapply(y, function(xx) ! is.null(xx$colNumber))
  
  expect_true(all(col_number_available))
})

test_that(".stopIfNotAllColumnsFound() works", {

  columnDescription <- list(
    x = list(match = "^x", colNumber = integer()),
    y = list(match = "^y", colNumber = 2)
  )

  expect_error(.stopIfNotAllColumnsFound(columnDescription, c("a", "y")))
})

test_that(".warnOnMultipleMatches() works", {

  columnDescription <- list(
    list(match = "^x", colNumber = 1),
    list(match = "^y", colNumber = 2:3)
  )
  
  expect_warning(.warnOnMultipleMatches(columnDescription, c("x", "y1", "y2")))
})
