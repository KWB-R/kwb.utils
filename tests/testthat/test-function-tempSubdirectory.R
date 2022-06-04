test_that("tempSubdirectory() works", {
  
  y <- tempSubdirectory("abc")
  
  expect_is(y, "character")
  expect_true(file.exists(y))
  expect_identical(basename(y), "abc")
})
