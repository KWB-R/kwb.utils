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
