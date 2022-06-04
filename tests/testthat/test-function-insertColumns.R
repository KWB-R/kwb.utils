test_that("insertColumns() works", {

  Data <- data.frame(A = 1:5, B = 2:6)

  X <- paste0("x", 1:5)
  Y <- paste0("y", 1:5)
  
  y_1 <- insertColumns(Data, before = "B", X = X, Y = Y)
  y_2 <- insertColumns(Data, after = "A", X = X, Y = Y)
  y_3 <- insertColumns(Data, before = "A", X = X, Y = X)
  y_4 <- insertColumns(Data, after = "B", X = X, Y = X)

  expect_identical(names(y_1), c("A", "X", "Y", "B"))
  expect_identical(y_2, y_1)
  
  expect_identical(names(y_3), c("X", "Y", "A", "B"))
  expect_identical(names(y_4), c("A", "B", "X", "Y"))
  
  expect_error(insertColumns(1))
  expect_error(insertColumns(Data, before = c("A", "B")))
  expect_error(insertColumns(Data, after = c("A", "B")))
  expect_error(insertColumns(Data, before = "A", after = "B"))
  
  expect_error(insertColumns(Data, after = "A", 77))
  expect_error(insertColumns(Data, after = "A", A1 = 77))
})
