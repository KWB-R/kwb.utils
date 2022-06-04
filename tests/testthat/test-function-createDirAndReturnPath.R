test_that("createDirAndReturnPath() gives a warning", {
  
  targetdir <- file.path(tempdir(), basename(tempfile(pattern = "test")))
  
  expect_warning(createDirAndReturnPath(targetdir, dbg = FALSE))
  expect_output(path <- createDirectory(targetdir), "already exists")
  expect_identical(path, targetdir)
})
