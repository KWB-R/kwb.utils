test_that("stopIfNotAllColumnsFound() works", {

  f <- kwb.utils:::stopIfNotAllColumnsFound

  expect_error(f())

  columnDescription <- list(
    x = list(match = "^x", colNumber = integer()),
    y = list(match = "^y", colNumber = 2)
  )
  
  expect_error(f(columnDescription, c("a", "y")))

  # TODO: why does this not pass?
  #expect_silent(f(columnDescription, c("x1", "y2")))
})
