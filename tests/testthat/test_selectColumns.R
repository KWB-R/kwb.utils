x1 <- data.frame(A = 1, B = 2, C = 3)
x2 <- data.frame(A = 1:2, B = 2:3, C = 3:4)

test_that("selectColumns returns a data frame for a one row data frame", {
  expect_equal(is.data.frame(selectColumns(x1, columns = c("A", "C"))), TRUE)
})

test_that("selectColumns returns a data frame if drop = FALSE", {
  results <- lapply(list(x1, x2), selectColumns, columns = "A", drop = FALSE)
  expect_equal(is.data.frame(results[[1]]), TRUE)
  expect_equal(is.data.frame(results[[2]]), TRUE)
})

test_that("selectColumns returns a vector for one column to select", {
  expect_equal(is.vector(selectColumns(x1, columns = "A")), TRUE)
  expect_equal(is.vector(selectColumns(x2, columns = "A")), TRUE)
})
