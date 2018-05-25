context("Encryption functions")

test_that("generateKeyFile() works",  {
  
  x <- generateKeyFile()
  
  expect_true(is.character(x))
  
  expect_length(x, 27)
  
  target <- tempfile()
  
  x <- generateKeyFile(target)
  
  expect_identical(x, readLines(target))
})

test_that(".checkNamespace() works", {

  expect_error(.checkNamespace("a", "b"))
  
  expect_null(.checkNamespace("kwb.utils", "selectColumns"))
})

test_that("createPasswordFile() and getPassword() work", {

  key_file <- tempfile(fileext = ".txt")
  
  password_file <- tempfile()
  
  on.exit(unlink(key_file))
  
  generateKeyFile(key_file)
  
  createPasswordFile("hauke", key_file, password_file, password = "abc")
  
  expect_identical(getPassword(password_file, key_file), "abc")
  
  expect_error(getPassword("no_such_file"))
  
  expect_output(.askForPassword("hauke"))
})
