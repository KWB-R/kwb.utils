skip_if(
  .OStype() != "windows", 
  message = "hsShell() and hsSystem() only available on Windows"
)

test_that("hsSystem() works", {
  
  expect_error(hsSystem())
  capture.output(y <- hsSystem("dir", intern = TRUE))
  expect_true(is.character(y))
})

test_that("hsShell() works", {
  
  expect_error(hsShell())
  expect_output(y <- hsShell("dir", intern = TRUE))
  expect_true(is.character(y))
})
