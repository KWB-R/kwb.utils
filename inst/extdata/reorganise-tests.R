#
# Reorganise test files: Make one file per tested function. This allows to more 
# easily find which test caused e.g. unwanted output.
#
# Author: Hauke Sonnenberg
# created: 2022-05-04

# Clear the environment
rm(list = ls())

# function names in kwb.utils
known_names <- kwb.utils::showPackageObjects()

# MAIN -------------------------------------------------------------------------
if (FALSE)
{
  # List "old" test files that were not created by kwb.test::create_test_files()
  files <- dir("tests/testthat", "^test_", full.names = TRUE)
  
  # Read the test files into lists of expressions. Give each expression a name and
  # provide the code as vector of character. 
  result <- lapply(stats::setNames(files, basename(files)), read_test)
  result <- lapply(result, kwb.utils::nameByElement, "name")
  
  # Skip files that we have already looked at
  result <- kwb.utils::removeElements(result, c(
    "test_column.R", 
    "test_encode_decode.R",
    "test_io.R",
    "test_merge.R",
    "test_pdf.R",
    "test_readArgs.R",
    "test_readCsvInputFile.R",
    "test_resolve.R",
    "test_selectColumns.R",
    "test_stat.R",
    "test_system.R",
    "test_string.R"
  ))

  # Extract tests into their own files and remove the original files...
  
  # for (r in result) {
  #   extract_tests_of_known_functions(x = r, known_names)
  # }
  
  #extract_tests_of_known_functions(x = result$test_findChanges.R, known_names)
  #extract_tests_of_known_functions(x = result$test_get_homedir.R, known_names)
  #extract_tests_of_known_functions(x = result$test_io.R, known_names)
  #extract_tests_of_known_functions(x = result$test_linearCombination.R, known_names)
  #extract_tests_of_known_functions(x = result$test_list.R, known_names)
  #extract_tests_of_known_functions(x = result$test_loaded.R, known_names)
  #extract_tests_of_known_functions(x = result$test_logical.R, known_names)
  #extract_tests_of_known_functions(x = result$test_lookup.R, known_names)
  #extract_tests_of_known_functions(x = result$test_main.R, known_names)
  #extract_tests_of_known_functions(x = result$test_matrix.R, known_names)
  #extract_tests_of_known_functions(x = result$test_merge.R, known_names)
  #extract_tests_of_known_functions(x = result$test_multiColumnLookup.R, known_names)
  #extract_tests_of_known_functions(x = result$test_naToLastNonNa.R, known_names)
  #extract_tests_of_known_functions(x = result$test_pairwise.R, known_names)
  #extract_tests_of_known_functions(x = result$test_parse.R, known_names)
  #extract_tests_of_known_functions(x = result$test_pdf.R, known_names)
  #extract_tests_of_known_functions(x = result$test_readArgs.R, known_names)
  #extract_tests_of_known_functions(x = result$test_readCsvInputFile.R, known_names)
  #extract_tests_of_known_functions(x = result$test_resolve.R, known_names)
  #extract_tests_of_known_functions(x = result$test_roundColumns.R, known_names)
  #extract_tests_of_known_functions(x = result$test_selectColumns.R, known_names)
  #extract_tests_of_known_functions(x = result$test_selectElements.R, known_names)
  #extract_tests_of_known_functions(x = result$test_stat.R, known_names)
  #extract_tests_of_known_functions(x = result$test_stat_2.R, known_names)
  #extract_tests_of_known_functions(x = result$test_string.R, known_names)
  #extract_tests_of_known_functions(x = result$test_subExpressionMatches.R, known_names)
  #extract_tests_of_known_functions(x = result$test_system.R, known_names)
  #extract_tests_of_known_functions(x = result$test_string.R, known_names)
  #extract_tests_of_known_functions(x = result$test_vector.R, known_names)
  extract_tests_of_known_functions(x = result$skip_if, known_names)
  
  # test_encryption.R
  x1 <- result$test_encryption.R
  x2 <- extract_test(x1, "generateKeyFile")
  x3 <- extract_test(x2, "createPasswordFile")
  x4 <- extract_test(x3, "checkNamespace")
  # Remove original file. Remaining context() call not required
  x4
  unlink(x1[[1L]]$file)
  
  # test_encode_decode.R -> skip

  # test_defaultIf.R
  x1 <- result$test_defaultIf.R
  x2 <- extract_test(x1, "_defaultIf_is_null")
  x3 <- extract_test(x2, "_defaultIf")
  x3 # check if empty
  unlink(x1[[1L]]$file)
  
  # test_default.R
  x1 <- result$test_default.R
  x2 <- extract_test(x1, "getFunctionValueOrDefault")
  x3 <- extract_test(x2, "defaultLevels")
  x4 <- extract_test(x3, "defaultIfNULL")
  x4 # check if empty
  unlink(x1[[1L]]$file)
  
  # test_dataFrame.R
  x1 <- result$test_dataFrame.R
  x2 <- extract_test(x1, "expandGrid")
  x3 <- extract_test(x2, "fullySorted")
  x4 <- extract_test(x3, "splitIntoFixSizedBlocks")
  x5 <- extract_test(x4, "resetRowNames")
  x6 <- extract_test(x5, "frequencyTable")
  x7 <- extract_test(x6, "compareDataFrames")
  x8 <- extract_test(x7, "compareSets")
  x9 <- extract_test(x8, "atLeastOneRowIn")
  x10 <- extract_test(x9, "rbindAll")
  x11 <- extract_test(x10, "safeRowBindOfListElements")
  x12 <- extract_test(x11, "safeRowBind")
  x13 <- extract_test(x12, "addRowWithName")
  x14 <- extract_test(x13, "moveToFront")
  x14 # check if empty
  unlink(x1[[1L]]$file)
  
  # test_convertCsvFile.R
  x1 <- result$test_convertCsvFile.R
  x2 <- extract_test(x1, "convertCsvFile")
  x2 # check if empty
  unlink(x1[[1L]]$file)

  # test_convert.R
  x1 <- result$test_convert.R
  x2 <- extract_test(x1, "intToNumeralSystem")
  x3 <- extract_test(x2, "toFactor")
  x4 <- extract_test(x3, "toPositiveIndices")
  x5 <- extract_test(x4, "toInches")
  x6 <- extract_test(x5, "limitToRange")
  x7 <- extract_test(x6, "toKeysAndValues")
  x8 <- extract_test(x7, "underscoreToPercent")
  x9 <- extract_test(x8, "toFormula")
  x10 <- extract_test(x9, "frenchToAscii")
  x11 <- extract_test(x10, "revertListAssignments")
  x12 <- extract_test(x11, "hsChrToNum")
  x13 <- extract_test(x12, "hsValidValue")
  x14 <- extract_test(x13, "hsStringToDouble")
  x15 <- extract_test(x14, "hsStringToDate")
  x15 # check if empty
  unlink(x1[[1L]]$file)
  
  # test_column.R
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

# extract_tests_of_known_functions ---------------------------------------------
extract_tests_of_known_functions <- function(x, known_names)
{
  #x <- result$test_io.R
  original_file <- x[[1L]]$file
  
  for (name in intersect(names(x), known_names)) {
    x <- extract_test(x, name)
  }
  
  if (length(x)) {
    # Overwrite original file with remaining code
    codes <- lapply(x, kwb.utils::selectElements, "code")
    text <- do.call(c, lapply(unname(codes), function(xx) c("", xx)))
    kwb.utils::catAndRun(
      paste("Rewriting", original_file), 
      writeLines(text, original_file)
    )
  } else {
    kwb.utils::catAndRun(
      paste("Removing", original_file), 
      unlink(original_file)
    )
  }
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
