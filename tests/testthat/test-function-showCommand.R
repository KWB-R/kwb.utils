test_that("showCommand() works", {

  f <- kwb.utils:::showCommand

  expect_error(f())
  expect_output(f("/path/to/program option1 option2 file"), "Running")
})
