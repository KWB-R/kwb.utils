test_that("dropUnusedFactorLevels() works", {
  
  data <- data.frame(
    id = 1:3,
    factor_1 = factor(c("a", "b", "a"), levels = c("a", "b", "c")),
    factor_2 = factor(c("x", "x", "y"), levels = c("x", "y", "z")),
    no_factor = c("A", "B", "C"),
    stringsAsFactors = FALSE
  )

  result <- dropUnusedFactorLevels(data, dbg = FALSE)
  
  check_column <- function(f) {
    expect_identical(sort(levels(f)), sort(unique(as.character(f))))
  }

  check_column(result$factor_1)
  check_column(result$factor_2)
})
