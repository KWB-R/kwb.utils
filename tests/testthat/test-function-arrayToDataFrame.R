library(testthat)

test_that("arrayToDataFrame() works", {
  
  dim_names <- list(
    upper = c("A", "B", "C"), 
    lower = c("x", "y"), 
    number = c("one", "two", "three", "four")
  )
  
  A <- array(
    seq_len(prod(lengths(dim_names))), 
    dimnames = dim_names, 
    dim = lengths(dim_names)
  )
  
  name <- "value"
  df <- arrayToDataFrame(A, name)
  
  expect_equal(nrow(df), prod(dim(A)))
  expect_identical(names(df), c(names(dim_names), name))
  expect_identical(df$upper[1:3], dim_names[[1L]])
  expect_identical(df$lower[1:3], rep("x", 3L))
  expect_identical(df$number[1:6], rep("one", 6L))
})
