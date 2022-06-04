test_that("listObjects() and loadObject() work", {
  
  file <- tempfile()
  
  a <- 1
  b <- data.frame(x = 1, y = 2)
  
  save(a, b, file = file)

  capture.output(y <- listObjects(file))
  capture.output(expect_warning(getNamesOfObjectsInRDataFiles(file)))
  
  expect_true(is.list(y))
  expect_identical(y[[1]], c("a", "b"))
  expect_identical(attr(y, "files"), file)
  
  expect_error(loadObject(file, dbg = FALSE))
  expect_error(loadObject(file, "no_such_object", dbg = FALSE))
  
  capture.output(expect_warning(getObjectFromRDataFile(file, "a")))
  expect_identical(loadObject(file, "a", dbg = FALSE), a)
  expect_identical(loadObject(file, "b", dbg = FALSE), b)
})
