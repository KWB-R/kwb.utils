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
  
  x <- data.frame(a = 1:5, b = 2:6, c = 3:7)

  y_1 <- moveColumnsToFront(x, "b")
  y_2 <- moveColumnsToFront(x, c("b", "a"))
  y_3 <- moveColumnsToFront(x)

  expect_identical(dim(x), dim(y_1))
  expect_identical(dim(x), dim(y_2))  

  expect_identical(names(y_1)[1], "b")
  expect_identical(names(y_2)[1:2], c("b", "a"))
  
  expect_identical(y_3, x)
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
    expect_identical(removeEmptyColumns(y), y)
    
    y$b <- NA
  
    expect_output(removeEmptyColumns(y))
    expect_identical(removeEmptyColumns(y, drop = TRUE), y$a)
  }
)

test_that("removeColumns() works", {

  x <- data.frame(a = 1:2, b = 2:3)
  
  expect_error(removeColumns(x))
  
  expect_identical(removeColumns(x, "z"), x)
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
})

# hsRenameColumns --------------------------------------------------------------

# renameColumns ----------------------------------------------------------------

# renameAndSelect --------------------------------------------------------------

# setColumns -------------------------------------------------------------------

#' # Create a data frame
#' x <- data.frame(a = 1:5)
#'
#' # Option 1: use the "$" operator
#' x1 <- x
#' x1$b <- 2:6
#' x1$c <- 3:7
#'
#' # Option 2: use setColumns
#' x2 <- setColumns(x, b = 2:6, c = 3:7)
#'
#' # The result is the same
#' identical(x1, x2)
#'
#' # but the creation of columns has been reported on the console (dbg = TRUE by
#' # default)
#'
#' ## Provide column 'b' to data frame 'x'... ok.
#' ## Provide column 'c' to data frame 'x'... ok.
#'   
# setColumns <- function(.x, ..., dbg = TRUE)
# stopifnot(is.data.frame(.x))
# stop("All column assignments be named!", call. = FALSE)
# catIf(dbg, sprintf("Provide column...
