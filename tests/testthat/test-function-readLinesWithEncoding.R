test_that("readLinesWithEncoding() works", {

  expect_error(
    readLinesWithEncoding()
    # argument "file" is missing, with no default
  )

})
