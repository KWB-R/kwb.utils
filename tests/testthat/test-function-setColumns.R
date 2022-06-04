test_that("setColumns() works", {

  x <- data.frame(a = 1:5)

  expect_error(setColumns(1, b = 2))
  
  expect_error(setColumns(x, 2))
  
  expect_silent(setColumns(x, b = 2:6, dbg = FALSE))

  expect_output(y <- setColumns(x, b = 2:6, c = 3:7))
  
  expect_identical(y, cbind(x, b = 2:6, c = 3:7))
})
