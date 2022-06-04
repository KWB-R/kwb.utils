test_that("frenchToAscii() works", {
  
  y <- frenchToAscii()
  
  expect_true(is.list(y))
  
  expect_true(all(grepl("^\\\\x[0-9a-fA-F]{2}", names(y))))
})
