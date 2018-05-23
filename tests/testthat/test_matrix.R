test_that("randomMatrix() works", {
  
  expect_true(is.matrix(randomMatrix()))
  
  dimension <- c(5L, 3L)
  
  expect_identical(dim(randomMatrix(dim = dimension)), dimension)
  
  values <- c(0, 0.5, 1, NA)
  
  expect_true(all(randomMatrix(dim = c(5, 3), values = values) %in% values))
})

test_that("createMatrix() works", {

  m1 <- createMatrix(c("A", "B", "C", "D", "E"), c("x", "y", "z"), -1)

  m2 <- createMatrix(c("A", "B", "C"))

  m3 <- createMatrix(c("A", "B", "C"), value = NA)

  m4 <- createMatrix(c("A", "B", "C"), name.row = "Letters")

  expect_true(all(sapply(list(m1, m2, m3, m4), is.matrix)))
  
  expect_true(all(m1 == -1))
  expect_true(all(m2 == 0))
  expect_true(all(is.na(m3)))
  expect_identical(names(dimnames(m4))[1], "Letters")
  
  expect_identical(colnames(m1), c("x", "y", "z"))
  expect_identical(rownames(m1), LETTERS[1:5])
  
  expect_identical(rownames(m2), LETTERS[1:3])
  expect_identical(rownames(m3), LETTERS[1:3])
  expect_identical(rownames(m4), LETTERS[1:3])
})
 
test_that("setMatrixColumns() works", {

  x <- createMatrix(c("a", "b", "c"))  
  
  y <- setMatrixColumns(x, list(c = 1:3))

  expect_warning(setMatrixColumns(x, list(r = 1)))
  
  expect_equal(unname(y[, "c"]), 1:3)
})

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
})

test_that("stopIfNotMatrix() works", {
  
  expect_error(stopIfNotMatrix(NULL))
  expect_error(stopIfNotMatrix(data.frame()))
  expect_silent(stopIfNotMatrix(matrix()))
})

test_that("diffrows() works", {

  x <- matrix(1:12, nrow = 3)

  d <- diffrows(x)

  expect_identical(nrow(d) + 1L, nrow(x))
  
  expect_equal(x[2, ] - x[1, ], d[1, ])
  expect_equal(x[3, ] - x[2, ], d[2, ])
})

test_that("asColumnList() and asRowList() work", {
  
  x <- matrix(1:12, nrow = 3)

  column_list <- asColumnList(x)
  row_list <- asRowList(x)
  
  expect_is(column_list, "list")
  expect_is(row_list, "list")
  
  expect_length(column_list, ncol(x))
  expect_length(row_list, nrow(x))
  
  for (i in 1:ncol(x)) {
    
    expect_identical(column_list[[i]], x[, i])
  }
  
  for (i in 1:nrow(x)) {
    
    expect_identical(row_list[[i]], x[i, ])
  }
})
