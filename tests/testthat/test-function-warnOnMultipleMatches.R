test_that(".warnOnMultipleMatches() works", {

  f <- kwb.utils:::warnOnMultipleMatches

  expect_error(f())
  
  columnDescription <- list(
    list(match = "^x", colNumber = 1),
    list(match = "^y", colNumber = 2:3)
  )
  
  expect_warning(f(columnDescription, c("x", "y1", "y2")))
})
