test_that("dropUnusedFactorLevels() works", {
  
  f <- dropUnusedFactorLevels
  
  expect_error(f(1))
  
  expect_output(f(data.frame(a = 1)), "No factor columns")
  
  data <- data.frame(
    id = 1:3,
    factor_1 = factor(c("a", "b", "a"), levels = c("a", "b", "c")),
    factor_2 = factor(c("x", "x", "y"), levels = c("x", "y", "z")),
    no_factor = c("A", "B", "C"),
    stringsAsFactors = FALSE
  )

  result <- f(data, dbg = FALSE)
  
  check_column <- function(f) {
    expect_identical(sort(levels(f)), sort(unique(as.character(f))))
  }

  check_column(result$factor_1)
  check_column(result$factor_2)
})
