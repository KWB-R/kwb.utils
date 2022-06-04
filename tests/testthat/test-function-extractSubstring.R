test_that("extractSubstring() works", {
  
  pattern <- "([^ ]+), ([0-9]+) of ([^ ]+)"
  
  datestring <- "Thursday, 8 of December"
  expect_identical(extractSubstring(pattern, datestring, 1), "Thursday")
  expect_identical(extractSubstring(pattern, datestring, 2), "8")
  expect_identical(extractSubstring(pattern, datestring, 3), "December")
  
  datestrings <- c("Thursday, 8 of December", "Tuesday, 14 of January")
  expect_identical(
    extractSubstring(pattern, datestrings, 1), c("Thursday", "Tuesday")
  )
  expect_identical(
    extractSubstring(pattern, datestrings, 2), c("8", "14")
  )
  expect_identical(
    extractSubstring(pattern, datestrings, 3), c("December", "January")
  )
  
  y1 <- extractSubstring(pattern, datestrings, 1:3)
  
  y2 <- extractSubstring(
    pattern, datestrings, index = c(weekday = 1, 2, month = 3)
  )

  expect_true(is.data.frame(y1))
  expect_true(is.data.frame(y2))

  expect_identical(dim(y1), c(2L, 3L))
  expect_identical(dim(y2), c(2L, 3L))
  
  expect_identical(names(y2)[c(1, 3)], c("weekday", "month"))
})
