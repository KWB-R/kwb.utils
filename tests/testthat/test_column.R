test_that("pasteColumns0() and pasteColumns() work", {

  x <- data.frame(A = 1:3, B = 2:4)
  
  y_1 <- pasteColumns0(x)
  y_2 <- pasteColumns(x, sep = ";")
  
  expect_is(y_1, "character")
  expect_is(y_2, "character")
  
  expect_identical(y_1, paste0(x$A, x$B))
  expect_identical(y_2, paste(x$A, x$B, sep = ";"))
  
  expect_error(pasteColumns0(x, "C"))
  expect_error(pasteColumns(x, "C"))
})

test_that("safeColumnBind() works", {

  x <- data.frame(a = 1:3, b = rnorm(3))

  expect_identical(safeColumnBind(NULL, x), x)
  
  expect_identical(safeColumnBind(x, x), cbind(x, x))
})

test_that("posixColumnAtPosition() and firstPosixColumn() work", {

  x_1 <- data.frame(a = 1)
  x_2 <- data.frame(DateTime = Sys.time())
  
  expect_warning(posixColumnAtPosition(x_1))  
  expect_true(posixColumnAtPosition(x_2) == 1L)
  
  expect_error(firstPosixColumn(1))
  expect_identical(firstPosixColumn(x_2), x_2$DateTime)
})

test_that("moveColumnsToFront() works", {
  
  df <- data.frame(a = 1:5, b = 2:6, c = 3:7)
  df_one_column <- data.frame(a = 1:4)
  
  y_1 <- moveColumnsToFront(df, "b")
  y_2 <- moveColumnsToFront(df, c("b", "a"))
  y_3 <- moveColumnsToFront(df)

  expect_identical(dim(df), dim(y_1))
  expect_identical(dim(df), dim(y_2))  

  expect_identical(names(y_1)[1], "b")
  expect_identical(names(y_2)[1:2], c("b", "a"))
  
  expect_identical(y_3, df)
  
  expect_identical(moveColumnsToFront(df_one_column, "a"), df_one_column)
})

test_that("checkForMissingColumns() works", {

  x <- data.frame(a = 1)
  
  expect_error(checkForMissingColumns(x, "b"))
  
  expect_warning(checkForMissingColumns(x, "b", do.stop = FALSE))
  
  expect_silent(checkForMissingColumns(x, "a"))
})

test_that(
  "hsAddMissingCols(), hsDelEmptyCols() and removeEmptyColumns() work", 
  {
    x <- data.frame(a = 1:2)
    y <- hsAddMissingCols(x, "b")
    
    expect_identical(names(y), c("a", "b"))
    expect_true(all(is.na(y$b)))
    
    expect_identical(hsDelEmptyCols(y), x)
    expect_identical(hsDelEmptyCols(y, drop = TRUE), x$a)
    
    y$b <- -1
    
    expect_identical(
      hsDelEmptyCols(y, FUN = function(xx) all(na.omit(xx) == -1), drop = TRUE),
      y$a
    )
    
    expect_output(removeEmptyColumns(y))
    expect_identical(removeEmptyColumns(y, dbg = FALSE), y)
    
    y$b <- NA
  
    expect_output(removeEmptyColumns(y))
    expect_identical(removeEmptyColumns(y, drop = TRUE, dbg = FALSE), y$a)
  }
)

test_that("removeColumns() works", {

  x <- data.frame(a = 1:2, b = 2:3, c = 3:4)
  
  expect_error(removeColumns(x))
  
  expect_warning(removeColumns(x, columnsToRemove = "a"))
  
  expect_identical(removeColumns(x, "z"), x)
  
  expect_identical(removeColumns(x, pattern = "^(a|c)$"), x[, "b", drop = FALSE])
  
  expect_output(removeColumns(x, c("a", "x"), dbg = TRUE), "column 'a' from")
})

test_that("insertColumns() works", {

  Data <- data.frame(A = 1:5, B = 2:6)

  X <- paste0("x", 1:5)
  Y <- paste0("y", 1:5)
  
  y_1 <- insertColumns(Data, before = "B", X = X, Y = Y)
  y_2 <- insertColumns(Data, after = "A", X = X, Y = Y)
  y_3 <- insertColumns(Data, before = "A", X = X, Y = X)
  y_4 <- insertColumns(Data, after = "B", X = X, Y = X)

  expect_identical(names(y_1), c("A", "X", "Y", "B"))
  expect_identical(y_2, y_1)
  
  expect_identical(names(y_3), c("X", "Y", "A", "B"))
  expect_identical(names(y_4), c("A", "B", "X", "Y"))
  
  expect_error(insertColumns(1))
  expect_error(insertColumns(Data, before = c("A", "B")))
  expect_error(insertColumns(Data, after = c("A", "B")))
  expect_error(insertColumns(Data, before = "A", after = "B"))
  
  expect_error(insertColumns(Data, after = "A", 77))
  expect_error(insertColumns(Data, after = "A", A1 = 77))
})

test_that("renameColumns() and renameAndSelect() work", {
  
  Data <- data.frame(A = 1:5, B = 2:6)
  
  renamings = list(A = "Alpha", B = "Bravo")
  
  y <- renameColumns(Data, renamings)

  expect_warning(hsRenameColumns(y, renamings))
  
  expect_identical(
    suppressWarnings(hsRenameColumns(y, renamings)), 
    renameColumns(y, renamings)
  )
  
  expect_identical(names(y), as.character(renamings))
  
  expect_identical(unname(Data), unname(y))
  
  expect_identical(Data, renameColumns(Data, NULL))
  
  expect_identical(renameAndSelect(Data, renamings), y)
})

test_that("setColumns() works", {

  x <- data.frame(a = 1:5)

  expect_error(setColumns(1, b = 2))
  
  expect_error(setColumns(x, 2))
  
  expect_silent(setColumns(x, b = 2:6, dbg = FALSE))

  expect_output(y <- setColumns(x, b = 2:6, c = 3:7))
  
  expect_identical(y, cbind(x, b = 2:6, c = 3:7))
})
