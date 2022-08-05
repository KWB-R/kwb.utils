test_that("extractSubstring() works", {

  f <- kwb.utils::extractSubstring  
  pattern <- "([^ ]+), ([0-9]+) of ([^ ]+)"
  
  x <- "Thursday, 8 of December"
  expect_identical(f(pattern, x, 1L), "Thursday")
  expect_identical(f(pattern, x, 2L), "8")
  expect_identical(f(pattern, x, 3L), "December")
  
  x <- c(
    "Thursday, 8 of December", 
    "Tuesday, 14 of January"
  )
  
  expect_identical(f(pattern, x, 1), c("Thursday", "Tuesday"))
  expect_identical(f(pattern, x, 2), c("8", "14"))
  expect_identical(f(pattern, x, 3), c("December", "January"))
  
  y1 <- f(pattern, x, 1:3)
  y2 <- f(pattern, x, index = c(weekday = 1, 2, month = 3))
  y3 <- f(pattern, x, 1:3, stringsAsFactors = TRUE)

  expect_true(is.data.frame(y1))
  expect_true(is.data.frame(y2))

  check_chr <- function(y) expect_true(all(sapply(y, is.character)))
  check_chr(y1)
  check_chr(y2)
  
  expect_true(all(sapply(y3, is.factor)))
  
  expect_identical(dim(y1), c(2L, 3L))
  expect_identical(dim(y2), c(2L, 3L))
  
  expect_identical(names(y2)[c(1, 3)], c("weekday", "month"))
  
})
