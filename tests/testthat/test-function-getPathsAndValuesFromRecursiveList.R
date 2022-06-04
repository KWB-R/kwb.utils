test_that("getPathsAndValuesFromRecursiveList() works", {
  
  # Define a recursive list
  x <- list(
    a = list(a1 = "A1", a2 = "A2"),
    b = list(b1 = "B1", b2 = "B2", b3 = "B3"),
    c = list(c1 = list(c11 = "C11"), c2 = list(c21 = "C21", c22 = "C22"))
  )
  
  # Get all non-list-elements and their "path" as a data frame
  y <- getPathsAndValuesFromRecursiveList(x)
  
  expect_true(is.data.frame(y))
  expect_identical(nrow(y), 8L)
  expect_identical(names(y), c("path", "value"))
  expect_equal(y$value, unname(unlist(x)))
  expect_equal(basename(y$path), tolower(y$value))
})
