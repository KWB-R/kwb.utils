test_that("firstElement() and lastElement() work", {

  x <- 1:10
  
  x_1 <- firstElement(x)
  x_n <- lastElement(x)
  
  expect_length(x_1, 1)
  expect_length(x_n, 1)
  
  expect_equal(x_1, 1)
  expect_equal(x_n, 10)
  
  expect_length(firstElement(numeric()), 0)
  expect_length(lastElement(numeric()), 0)
})
