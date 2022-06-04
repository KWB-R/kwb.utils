test_that("objectSize() works", {

  capture <- capture.output
  
  capture(results <- list(
    objectSize(character()), 
    objectSize(integer()), 
    objectSize(double())
  ))
  
  capture(expect_true(all(sapply(results, grepl, pattern = "bytes$"))))
  capture(expect_true(grepl("Kb$", objectSize(1:100, units = "KB"), ignore.case = TRUE)))
  capture(expect_error(objectSize(1:100, units = "no_such_unit")))
  
  capture(y <- objectSize(list(
    a = list(x = 1:20, letters = LETTERS),
    b = 77
  )))
  
  expect_is(y, "list")
})
