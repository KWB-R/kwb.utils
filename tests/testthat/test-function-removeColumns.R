test_that("removeColumns() works", {

  x <- data.frame(a = 1:2, b = 2:3, c = 3:4)
  
  expect_error(removeColumns(x))
  
  expect_warning(removeColumns(x, columnsToRemove = "a"))
  
  expect_identical(removeColumns(x, "z"), x)
  
  expect_identical(removeColumns(x, pattern = "^(a|c)$"), x[, "b", drop = FALSE])
  
  expect_output(removeColumns(x, c("a", "x"), dbg = TRUE), "column 'a' from")
})
