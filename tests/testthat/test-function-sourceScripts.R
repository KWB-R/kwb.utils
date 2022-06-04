test_that("sourceScripts() works", {

  temp_file_1 <- tempfile()
  temp_file_2 <- tempfile()
  
  writeLines("cat('Hello, world!')", temp_file_1)
  writeLines("x <- 123; stop('Hello, error!')", temp_file_2)
  
  expect_output(sourceScripts(temp_file_1, dbg = FALSE))
  expect_error(sourceScripts(temp_file_2, dbg = FALSE))
  expect_identical(x, 123)
  
  expect_warning(sourceScripts("no_such_script", dbg = FALSE))
})
