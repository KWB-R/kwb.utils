test_that("extractRowRanges() works", {

  dataFrame <- as.data.frame(stringsAsFactors = FALSE, matrix(
    ncol = 2, byrow = TRUE, c(
      "Date", "Value",
      "1.1.", "1",
      "2.1.", "2",
      "", "",
      "Date", "Value",
      "3.1.", "3",
      "4.1.", "4"
    )
  ))
  
  arguments <- list(
    dataFrame, columnName = "V1", pattern = "Date", stopOffset = 2
  )
  
  y_1 <- callWith(extractRowRanges, arguments)
  
  y_2 <- callWith(extractRowRanges, arguments, nameByMatch = TRUE)
  
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
  
  expect_identical(y_1, expected)
  
  expect_named(y_2)
})

test_that("startsToRanges() works", {
  
  starts <- c(1, 10, 20, 35)
  
  expect_identical(startsToRanges(starts, lastStop = 50), data.frame(
    from = c(2, 11, 21, 36),
    to = c(9, 19, 34, 50)
  ))
  
  expect_identical(
    startsToRanges(starts, lastStop = 55, startOffset = 2, stopOffset = 2),
    data.frame(
      from = c(3, 12, 22, 37),
      to = c(8, 18, 33, 55)
    )
  )
})

test_that("startsToEnds() works", {
  
  starts <- c(1, 10, 20, 35)
  
  expect_identical(startsToEnds(starts, lastStop = 50), c(9, 19, 34, 50))
  
  expect_identical(
    startsToEnds(starts, lastStop = 50, stopOffset = 2), 
    c(8, 18, 33, 50)
  )
})
