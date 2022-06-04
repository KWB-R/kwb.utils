test_that("assertRowsAndColumns() works", {
  
  m <- matrix(1:12, nrow = 3, ncol = 4, dimnames = list(
    rows = paste0("row", 1:3), cols = paste0("col", 1:4)
  ))

  # Add two rows, reverse order of rows, add one column, remove one column
  row_names <- paste0("row", 4:0)
  col_names <- paste0("col", 0:2)
  
  y <- assertRowsAndColumns(m, row_names = row_names, col_names = col_names)

  expect_is(y, "matrix")
  
  expect_identical(rownames(y), row_names)
  expect_identical(colnames(y), col_names)
  
  m_unnamed <- unname(m)
  
  expect_identical(assertRowsAndColumns(m_unnamed), m_unnamed)
})
