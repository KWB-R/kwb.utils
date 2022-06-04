test_that("posixColumnAtPosition() and firstPosixColumn() work", {

  x_1 <- data.frame(a = 1)
  x_2 <- data.frame(DateTime = Sys.time())
  
  expect_warning(posixColumnAtPosition(x_1))  
  expect_true(posixColumnAtPosition(x_2) == 1L)
  
  expect_error(firstPosixColumn(1))
  expect_identical(firstPosixColumn(x_2), x_2$DateTime)
})
