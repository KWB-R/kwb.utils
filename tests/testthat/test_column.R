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
