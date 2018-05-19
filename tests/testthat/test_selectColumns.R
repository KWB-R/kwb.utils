x1 <- data.frame(A = 1, B = 2, C = 3)
x2 <- data.frame(A = 1:2, B = 2:3, C = 3:4)

test_that("selectColumns returns a data frame for a one row data frame", {
  expect_true(is.data.frame(selectColumns(x1, columns = c("A", "C"))))
})

test_that("selectColumns returns a data frame if drop = FALSE", {
  results <- lapply(list(x1, x2), selectColumns, columns = "A", drop = FALSE)
  expect_true(is.data.frame(results[[1]]))
  expect_true(is.data.frame(results[[2]]))
})

test_that("selectColumns returns a vector for one column to select", {
  expect_true(is.vector(selectColumns(x1, columns = "A")))
  expect_true(is.vector(selectColumns(x2, columns = "A")))
})

test_that("selectColums() works", {
   f <- selectColumns
   d <- data.frame(a = 1, b = "one")
   expect_error(f())
   expect_error(f(1))
   expect_error(f(list()))
   expect_equal(f(d, "a"), 1)
   expect_equal(f(d, "b"), as.factor("one"))
})
