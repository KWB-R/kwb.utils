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

test_that("sourceScripts() works", {

  temp_file_1 <- tempfile()
  temp_file_2 <- tempfile()
  
  writeLines("cat('Hello, world!')", temp_file_1)
  writeLines("x <- 123; stop('Hello, error!')", temp_file_2)
  
  expect_output(sourceScripts(temp_file_1, dbg = FALSE))
  expect_error(sourceScripts(temp_file_2, dbg = FALSE))
  
  expect_identical(x, 123)
})

test_that("runInDirectory() works", {
  
  path <- system.file(package = "kwb.utils")

  expect_identical(runInDirectory(path, dir), dir(path))
  expect_output(runInDirectory(path, dir, .dbg = TRUE))
})

test_that("defaultWindowsProgramFolders() works", {

  y <- defaultWindowsProgramFolders()
  
  expect_is(y, "character")
  expect_named(y)
})

test_that("mySystemTime() works", {
  
  expect_output(y <- mySystemTime(max, list(1:10)))
  
  expect_is(y, "integer")
  expect_length(y, 1)
})

# runBatchfileInDirectory ------------------------------------------------------

test_that("cmdLinePath() works", {

  expect_error(cmdLinePath())
  
  y <- cmdLinePath("x/y/z")
  
  expect_is(y, "character")
  
  expect_true(grepl('^".*"$', y))
})

test_that("copyDirectoryStructure() works", {
  
  sourcedir <- system.file(package = "kwb.utils")

  folder_paths <- function(x) list.dirs(x, recursive = TRUE, full.names = FALSE)

  targetdir <- createDirectory(file.path(tempdir(), "test_copy"))

  copyDirectoryStructure(sourcedir, targetdir)
  
  expect_identical(folder_paths(targetdir), folder_paths(sourcedir))
})

test_that("createDirAndReturnPath() gives a warning", {
  targetdir <- file.path(tempdir(), "test_createDir")
  
  expect_warning(path <- createDirAndReturnPath(targetdir))
  
  expect_identical(path, createDirectory(targetdir))
})

test_that("createDirectory() works", {
  
  targetdir <- file.path(tempdir(), "test_createDir")
  
  path <- createDirectory(targetdir)
  
  expect_true(file.exists(targetdir))
  
  expect_identical(path, targetdir)
})

test_that("tempSubdirectory() works", {
  
  y <- tempSubdirectory("abc")
  
  expect_is(y, "character")
  expect_true(file.exists(y))
  expect_identical(basename(y), "abc")
})

# hsOpenWindowsExplorer --------------------------------------------------------

test_that(".isNetworkPath() works", {

  expect_false(.isNetworkPath("abc"))
  expect_true(.isNetworkPath("//abc"))
  expect_true(.isNetworkPath("//abc/def/ghi"))
})

test_that("windowsPath() and rStylePath() work", {

  x <- "a/b/c"
  y <- windowsPath(x)
  
  expect_identical(hsCountInStr("\\\\", y), hsCountInStr("/", x))
  expect_identical(rStylePath(y), x)
})

test_that(".showCommand() works", {

  expect_error(.showCommand())
  
  expect_output(.showCommand("/path/to/program option1 option2 file"))
})

test_that("hsSystem() works", {

  expect_error(hsSystem())
  
  expect_output(y <- hsSystem("dir"))
  
  expect_identical(y, 0L)
})

test_that("hsShell() works", {

  if (.OStype() == "windows") {

    expect_error(hsShell())
    
    expect_output(y <- hsShell("dir"))
    
    expect_identical(y, 0L)
  }
})

