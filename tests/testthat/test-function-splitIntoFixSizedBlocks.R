test_that("splitIntoFixSizedBlocks() works", {

  y <- splitIntoFixSizedBlocks(iris, 50)
  
  expect_true(is.list(y))
  
  expect_true(all(sapply(y, is.data.frame)))
  
  expect_identical(sum(sapply(y, nrow)), nrow(iris))
  
  for (column in names(iris)) {
  
    expect_true(all(unlist(lapply(y, "[[", column)) == iris[[column]]))
  }
})
