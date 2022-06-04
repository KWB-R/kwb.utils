test_that("checkForMissingColumns() works", {

  x <- data.frame(a = 1)
  
  expect_error(checkForMissingColumns(x, "b"))
  
  expect_warning(checkForMissingColumns(x, "b", do.stop = FALSE))
  
  expect_silent(checkForMissingColumns(x, "a"))
})
