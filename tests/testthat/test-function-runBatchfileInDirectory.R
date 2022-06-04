test_that("runBatchfileInDirectory() works", {

  directory <- tempSubdirectory("test_batch")
  
  batchfile <- file.path(directory, "test.bat")
  
  writeLines("echo Hello, world", batchfile)
  
  old_dir <- getwd()
  
  if (.OStype() == "windows") {
    
    runBatchfileInDirectory(batchfile, directory)
    
    expect_identical(old_dir, getwd())
    
  } else {
    
    expect_error(runBatchfileInDirectory(batchfile, directory))
  }
})
