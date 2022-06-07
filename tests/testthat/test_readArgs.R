test_that("Argument lists are read correctly", {
  
  content <- c(
    "id,a,b,c,additional.args",
    "c1,1,2,3,",
    "c2,{2},3,4,", 
    "c3,'x','y','z','c1'"
  )
  
  temp_file <- tempfile(fileext = ".csv")
  
  writeLines(content, temp_file)
  
  x <- kwb.utils:::readArglistsTable.csv(temp_file)
  
  expect_length(x, length(strsplit(content, ",")[[1]]))
  expect_equal(nrow(x), length(content) - 1)
  
  y <- readArglists(configTable = x)
  
  expect_length(y, nrow(x))
  expect_named(y)
  expect_equal(names(y), x$id)
  expect_true(allAreEqual(lapply(y, function(yy) {
    setdiff(names(yy), "additional.args")
  })))
})
