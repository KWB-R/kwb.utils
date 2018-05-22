test_that("objectSize() works", {

  results <- list(
    objectSize(character()), 
    objectSize(integer()), 
    objectSize(double())
  )
  
  expect_true(all(sapply(results, grepl, pattern = "bytes$")))
  
  expect_true(grepl("Kb$", objectSize(1:100, units = "KB"), ignore.case = TRUE))
  
  expect_error(objectSize(1:100, units = "no_such_unit"))
})

test_that("safePath() works", {

  expect_error(safePath("no", "such", "path"))
  
  temp_file <- tempfile()
  on.exit(unlink(temp_file))
  
  writeLines("Hello, world!", temp_file)
  
  expect_identical(safePath(temp_file), temp_file)
})

test_that(".OStype() returns 'unix' or 'windows'", {

  expect_true(.OStype() %in% c("unix", "windows"))
})

test_that("desktop() and user() work", {
  
  user_name <- user()
  path <- desktop()
  
  expect_true(is.character(user_name))
  expect_true(is.character(path))
  
  expect_length(user_name, 1)
  expect_length(path, 1)
  
  expect_true(grepl("desktop", path, ignore.case = TRUE))
  expect_true(grepl(user_name, path, ignore.case = TRUE))
})

# sourceScripts ----------------------------------------------------------------
# runInDirectory ---------------------------------------------------------------
# defaultWindowsProgramFolders -------------------------------------------------
# mySystemTime -----------------------------------------------------------------
# runBatchfileInDirectory ------------------------------------------------------
# cmdLinePath ------------------------------------------------------------------
# copyDirectoryStructure -------------------------------------------------------
# createDirAndReturnPath -------------------------------------------------------
# createDirectory --------------------------------------------------------------
# tempSubdirectory -------------------------------------------------------------
# hsOpenWindowsExplorer --------------------------------------------------------
# .isNetworkPath ---------------------------------------------------------------
# windowsPath ------------------------------------------------------------------
# rStylePath -------------------------------------------------------------------
# .showCommand -----------------------------------------------------------------
# hsSystem ---------------------------------------------------------------------
# hsShell ----------------------------------------------------------------------
