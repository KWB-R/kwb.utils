test_that("compareDataFrames() works", {

  x <- data.frame(a = 1:2, b = 2:3)
  y <- x
  
  expect_true(all(unlist(compareDataFrames(x, y))))
  
  z <- compareDataFrames(x, y[, c("b", "a")])
  
  expect_identical(
    names(which(! unlist(z))),
    c("identical", "identicalExceptAttributes", "sameColumnNames")
  )
  
  expect_output(tmp <- compareDataFrames(x, y, dbg = TRUE))
})
