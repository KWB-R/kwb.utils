test_that("Argument lists are read correctly", {
  
  content <- c(
    "id,a,b,c",
    "c1,1,2,3",
    "c2,{2},3,4", 
    "c3,'x','y','z'"
  )
  
  temp_file <- tempfile()
  
  writeLines(content, temp_file)
  
  x <- kwb.utils:::.readArglistsTable.csv(temp_file)
  
  expect_length(x, length(content))
  
  y <- readArglists(configTable = x)
  
  expect_length(y, nrow(x))
  expect_named(y)
  expect_equal(names(y), x$id)
  expect_true(allAreEqual(lapply(y, names)))
})
