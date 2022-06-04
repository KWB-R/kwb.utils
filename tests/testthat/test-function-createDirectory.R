test_that("createDirectory() works", {
  
  targetdir_1 <- file.path(tempdir(), "test_createDir")
  targetdir_2 <- file.path(tempdir(), "test/create/Dir")
  
  for (targetdir in list(targetdir_1, targetdir_2)) {
    
    path <- createDirectory(targetdir, dbg = FALSE)
    
    expect_true(file.exists(targetdir))
    
    expect_identical(path, targetdir)
  }
})
