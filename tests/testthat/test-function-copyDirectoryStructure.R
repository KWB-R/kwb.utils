test_that("copyDirectoryStructure() works", {
  
  sourcedir <- system.file(package = "kwb.utils")

  folder_paths <- function(x) list.dirs(x, recursive = TRUE, full.names = FALSE)

  targetdir <- createDirectory(file.path(tempdir(), "test_copy"), dbg = FALSE)

  expect_output(copyDirectoryStructure(sourcedir, targetdir))
  expect_identical(folder_paths(targetdir), folder_paths(sourcedir))
})
