test_that("extractRowRanges() works", {

  textLines <- c(
    "Date,Value",
    "1.1.,1",
    "2.1.,2",
    ",",
    "Date,Value",
    "3.1.,3",
    "4.1.,4"
  )

  # Convert textLines to data frame. The function should be able to handle both.
  dataFrame <- read.table(text = textLines, sep = ",", stringsAsFactors = FALSE)

  # Define expected result
  expected <- list(
    data.frame(
      Date = c("1.1.", "2.1."),
      Value = c("1", "2"),
      stringsAsFactors = FALSE
    ),
    data.frame(
      Date = c("3.1.", "4.1."),
      Value = c("3", "4"),
      stringsAsFactors = FALSE
    )
  )
  
  arguments <- list(
    pattern = "Date", column = "V1", stopOffset = 2L
  )
  
  y1 <- callWith(extractRowRanges, arguments, x = dataFrame)
  y2 <- callWith(extractRowRanges, arguments, x = dataFrame, nameByMatch = TRUE)

  expect_identical(expected, y1)
  expect_named(y2)
  expect_identical(expected, unname(y2))
  
  y3 <- callWith(extractRowRanges, arguments, x = textLines)
  y4 <- callWith(extractRowRanges, arguments, x = textLines, nameByMatch = TRUE)

  expectedText <- lapply(expected, pasteColumns, sep = ",")
  
  expect_identical(expectedText, y3)
  expect_named(y4)
  expect_identical(expectedText, unname(y4))
})
