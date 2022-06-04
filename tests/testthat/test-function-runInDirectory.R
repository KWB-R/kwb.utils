test_that("runInDirectory() works", {
  
  path <- system.file(package = "kwb.utils")

  expect_identical(runInDirectory(path, dir), dir(path))
  expect_output(runInDirectory(path, dir, .dbg = TRUE))
})
