test_that("desktop() and user() work", {
  
  user_name <- user()
  path <- desktop()
  
  expect_true(is.character(user_name))
  expect_true(is.character(path))
  
  expect_length(user_name, 1)
  expect_length(path, 1)
  
  expect_true(grepl("desktop", path, ignore.case = TRUE))
  expect_true(grepl(user_name, path, ignore.case = TRUE))
  
  expect_error(desktop("no_such_os"))
  expect_error(user("no_such_os"))
  
  expect_true(grepl("Desktop$", desktop("windows")))
})
