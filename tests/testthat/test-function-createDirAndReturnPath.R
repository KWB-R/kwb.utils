test_that("createDirAndReturnPath() gives a warning", {
  
  targetdir <- file.path(tempdir(), "test_createDir")
  
  expect_warning(path <- createDirAndReturnPath(targetdir, dbg = FALSE))
  expect_identical(path, createDirectory(targetdir, dbg = FALSE))
})
