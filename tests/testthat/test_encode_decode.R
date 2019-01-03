#library(testthat)
#kwb.utils::assignPackageObjects("kwb.utils")

context("test-encode_decode")

test_that("encoding and decoding works", {
  
  check_coding <- function(x) expect_identical(decode(encode(x)), x)
  
  check_coding(x = c("Hallo", "Du", "dummer", "Esel", "Hallo"))
  check_coding(x = as.character(100000:100100))
  check_coding(x = as.character(rnorm(10000)))
})
