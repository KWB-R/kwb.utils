test_that("safeRowBind() and safeRowBindAll() work", {
  
  data_1 <- data.frame(A = 1:2, B = 2:3)
  data_2 <- data.frame(B = 3:4, C = 4:5)
  
  y <- safeRowBind(data_1, data_2)
  
  expect_true(is.data.frame(y))
  expect_identical(dim(y), c(4L, 3L))
  expect_identical(names(y), c("A", "B", "C"))
  
  expect_error(safeRowBind(1, 1))
  expect_error(safeRowBind(data.frame(a = 1), 1))
  
  expect_identical(safeRowBind(data_1, NULL), data_1)
  expect_identical(safeRowBind(NULL, data_2), data_2)
  
  expect_identical(safeRowBindAll(list(data_1, data_2)), y)
})
