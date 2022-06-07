test_that("findPartialDuplicates() works", {

  f <- kwb.utils:::findPartialDuplicates

  expect_error(f())

  data1 <- data.frame(a = 1:2, b = 2:3)
  data2 <- data.frame(a = c(1L, 1L), b = 2:3, c = c("same", "same"))
  
  expect_null(f(data1, key_columns = "a"))

  expect_identical(f(data2, "a"), list(data2[, 1:2]))
})
