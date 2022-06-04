test_that("hsValidValue() works", {
  
  expect_false(hsValidValue("1.1", "de"))
  expect_false(hsValidValue("1,1", "en"))
  
  expect_true(hsValidValue("1.1", "en"))
  expect_true(hsValidValue("1,1", "de"))
  
  x <- c("1.1", "2,2", "3.3", NA)
  
  expect_identical(hsValidValue(x, "en"), c(TRUE, FALSE, TRUE, TRUE))
  
  expect_identical(
    hsValidValue(x, "en", accept.na = FALSE), c(TRUE, FALSE, TRUE, FALSE)
  )
                   
  expect_output(hsValidValue(x, "de", dbg = TRUE))
})
