test_that("callWithStringsAsFactors() works", {

  get_stringsAsFactors <- function() getOption("stringsAsFactors")
  
  option_bak <- get_stringsAsFactors()

  d1 <- callWithStringsAsFactors(TRUE, get_stringsAsFactors)
  expect_identical(d1, TRUE)
  expect_identical(option_bak, get_stringsAsFactors())
  
  d2 <- callWithStringsAsFactors(FALSE, get_stringsAsFactors)
  expect_identical(d2, FALSE)
  expect_identical(option_bak, get_stringsAsFactors())

  expect_error(callWithStringsAsFactors(1, data.frame, a = "x"))
})
