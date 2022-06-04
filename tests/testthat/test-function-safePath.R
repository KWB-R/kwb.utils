test_that("safePath() works", {

  expect_error(safePath("no", "such", "path"))
  
  temp_file <- tempfile()
  on.exit(unlink(temp_file))
  
  writeLines("Hello, world!", temp_file)
  
  expect_identical(safePath(temp_file), temp_file)
})
