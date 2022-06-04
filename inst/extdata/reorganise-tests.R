#
# Reorganise test files: Make one file per tested function. This allows to more 
# easily find which test caused e.g. unwanted output.
#
# Author: Hauke Sonnenberg
# created: 2022-05-04

# Clear the environment
rm(list = ls())

# MAIN -------------------------------------------------------------------------
if (FALSE)
{
  # List "old" test files that were not created by kwb.test::create_test_files()
  files <- dir("tests/testthat", "^test_", full.names = TRUE)
  
  # Read the test files into lists of expressions. Give each expression a name and
  # provide the code as vector of character. 
  result <- lapply(stats::setNames(files, basename(files)), read_test)
  result <- lapply(result, kwb.utils::nameByElement, "name")
  
  # Extract tests into their own files and remove the original files
  x1 <- result$test_column.R
  x2 <- extract_test(x1, "pasteColumns0")
  x3 <- extract_test(x2, "safeColumnBind")
  x4 <- extract_test(x3, "posixColumnAtPosition")
  x5 <- extract_test(x4, "moveColumnsToFront")
  x6 <- extract_test(x5, "checkForMissingColumns")
  x7 <- extract_test(x6, "removeColumns")
  x8 <- extract_test(x7, "insertColumns")
  x9 <- extract_test(x8, "renameColumns")
  x10 <- extract_test(x9, "setColumns")
  # Overwrite original file with remaining code
  writeLines(x10$test_that$code, x10$test_that$file)
  
  # test_blockRange.R
  x1 <- result$test_blockRange.R
  x2 <- extract_test(x1, "extractRowRanges")
  x3 <- extract_test(x2, "startsToRanges")
  x4 <- extract_test(x3, "startsToEnds")
  x4  # check if empty
  unlink(x1[[1L]]$file)
  
  # test_attribute.R: 
  x1 <- result$test_attribute.R
  x2 <- extract_test(x1, "addClass")
  x3 <- extract_test(x2, "hsRestoreAttributes")
  x4 <- extract_test(x3, "getAttribute")
  x5 <- extract_test(x4, "removeAttributes")
  x5 # check if empty
  unlink(x1[[1L]]$file)
  
  # test_arglist.R
  x1 <- result$test_arglist.R
  x2 <- extract_test(x1, "arglist")
  x3 <- extract_test(x2, "mergeLists")
  unlink(x1[[1L]]$file)
}

# read_test --------------------------------------------------------------------
read_test <- function(file)
{
  #file <- files[1L]
  cat("Reading", file, "\n")
  exprs <- parse(file, keep.source = TRUE)
  lapply(seq_along(exprs), function(i) {
    #i <- 1L
    # Important to use [i] instead of [[i]] here, otherwise the original 
    # formatting gets lost!
    rawcode <- deparse(exprs[i], control = "useSource")
    # Remove top level "expression()"
    code <- rawcode
    code[1L] <- gsub("^expression\\(", "", code[1L])
    code[length(code)] <- gsub("\\)$", "", code[length(code)])
    name <- strsplit(code[1L], " ")[[1L]][1L]
    name <- gsub("^test_that\\(\"", "", name)
    name <- kwb.utils::substSpecialChars(name)
    list(name = name, code = code, file = file)
  })
}

# extract_test -----------------------------------------------------------------
extract_test <- function(x, name)
{
  #x <- result$test_arglist.R
  #name <- "arglist"
  test <- kwb.utils::selectElements(x, name)
  file <- kwb.test:::path_to_testfile("tests/testthat", test$name)
  kwb.utils::catAndRun(
    paste("Writing", file), 
    writeLines(test$code, file)
  )
  
  x[names(x) != name]
}
