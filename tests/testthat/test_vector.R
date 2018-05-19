test_that("enlargeVector() works", {

  expect_error(enlargeVector(1:10, 5))
  
  v1 <- enlargeVector(1:5, 10, fill.with = 0) 
  v2 <- enlargeVector(1:5, 10, fill.with = NA) 
  v3 <- enlargeVector(c("a", "b", "c"), 10) 
  v4 <- enlargeVector(c("a", "b", "c"), 10, fill.with = "?")

  expect_length(v1, 10)
  expect_length(v2, 10)
  expect_length(v3, 10)
  expect_length(v4, 10)

  expect_equal(sum(v1 == 0), 5L)
  expect_equal(sum(is.na(v2)), 5L)  
  expect_equal(sum(v3 == ""), 7L)
  expect_equal(sum(v4 == "?"), 7L)
}) 

test_that("recycle() works", {

  x <- 1:10
  y <- recycle(x, 20)
  
  expect_equal(recycle(x), x)
  expect_length(y, 20)
  expect_equal(y[1:10], y[11:20])
})

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

test_that("getByPositiveOrNegativeIndex() works", {

  f <- getByPositiveOrNegativeIndex
  x <- 1:10
  
  expect_error(f(x, 11))
  
  expect_equal(f(x, -1), tail(x, 1))
  expect_equal(f(x,  1), head(x, 1))
})
