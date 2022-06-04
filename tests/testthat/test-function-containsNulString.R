test_that("containsNulString() works", {
  
  file <- tempfile()
  
  writeBin(as.raw(c(0xff, 0xfe)), con = file)
  
  expect_true(containsNulString(file))
  
  writeBin(as.raw(c(0x55, 0xff, 0xfe)), con = file)
  
  expect_false(containsNulString(file))
})
