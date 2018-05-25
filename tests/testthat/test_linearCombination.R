x <- randomMatrix(c(10, 10))

coeffs <- sample(ncol(x))

test_that("the linear combination is the same for version 1 and 2", {
  
  expect_equal(linearCombination(x, coeffs, version = 1), 
               linearCombination(x, coeffs, version = 2))
})

test_that("the linear combination is calculated correctly", {
  
  expect_equal(linearCombination(x, coeffs), 
               sapply(seq_len(nrow(x)), function(i) sum(x[i, ] * coeffs)))
})

test_that("the length of the result equals the number of rows", {
  
  expect_equal(length(linearCombination(x, coeffs)), 
               nrow(x))
})

test_that("the number of coefficients equals the number of columns", {
  
  expect_error(linearCombination(x, coeffs[-1]))
})

test_that("linearCombination() works with a data frame", {
  
  expect_identical(
    linearCombination(as.data.frame(x), coeffs),
    linearCombination(x, coeffs)
  )
})

test_that("linearCombination() does not accept non-numerical arguments", {
  
  expect_error(linearCombination(matrix(LETTERS, nrow = 2), coeffs))
  expect_error(linearCombination(x, LETTERS[1:ncol(x)]))
})
