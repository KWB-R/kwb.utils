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
 
# setMatrixColumns -------------------------------------------------------------
# assertRowsAndColumns ---------------------------------------------------------
#' m <- matrix(1:12, nrow = 3, ncol = 4, dimnames = list(
#'   rows = paste0("row", 1:3), cols = paste0("col", 1:4)
#' ))
#'
#' # Add two rows, reverse order of rows, add one column, remove one column
#' assertRowsAndColumns(
#'   m,
#'   row_names = paste0("row", 4:0),
#'   col_names = paste0("col", 0:2)
#' )

# stopIfNotMatrix --------------------------------------------------------------
# diffrows ---------------------------------------------------------------------
#' x <- matrix(1:12, nrow = 3)
#' 
#' d <- diffrows(x)
#' 
#' x[2, ] - x[1, ] == d[1, ]
#' x[3, ] - x[2, ] == d[2, ]

# asColumnList -----------------------------------------------------------------
#' x <- matrix(1:12, nrow = 3)
#' 
#' column_list <- asColumnList(x)
#' 
#' for (i in 1:ncol(x)) print(identical(column_list[[i]], x[, i]))

# asRowList --------------------------------------------------------------------
#' x <- matrix(1:12, nrow = 3)
#' 
#' row_list <- asRowList(x)
#' 
#' for (i in 1:nrow(x)) print(identical(row_list[[i]], x[i, ]))
