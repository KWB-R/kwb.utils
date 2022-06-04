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
