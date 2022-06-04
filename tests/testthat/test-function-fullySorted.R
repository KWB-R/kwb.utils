test_that("fullySorted() works", {
  
  y_1 <- fullySorted(head(iris))
  y_2 <- fullySorted(head(iris), renumber.rows = FALSE)
  y_3 <- fullySorted(head(iris), decreasing = TRUE)
  
  expect_false(is.unsorted(as.integer(rownames(y_1))))
  expect_true(is.unsorted(as.integer(rownames(y_2))))
  
  expect_identical(y_1, resetRowNames(y_3[seq(nrow(y_3), 1), ]))
})
