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
  
   d <- data.frame(a = 1, b = "one", stringsAsFactors = TRUE)
   
   expect_error(selectColumns())
   expect_error(selectColumns(1))
   expect_error(selectColumns(list()))
   expect_equal(selectColumns(d, "a"), 1)
   expect_equal(selectColumns(d, "b"), as.factor("one"))
   
   expect_identical(selectColumns(d, NULL), d)
   
   expect_warning(selectColumns(d, "c", do.stop = FALSE))
   
   expect_identical(selectColumns(d, pattern = "^b$"), d$b)
})
