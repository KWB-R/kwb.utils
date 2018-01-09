# Create a random matrix of integer values
M1 <- matrix(sample(100, 12), nrow = 4, dimnames = list(LETTERS[1:4], 1:3))

# Introduce some NA
values <- as.numeric(M1)
values[sample(length(values), 3)] <- NA
M2 <- matrix(values, nrow = nrow(M1), dimnames = dimnames(M1))

test_that("columnwisePercentage() works as expected", {
  
  P1 <- columnwisePercentage(M1, digits = NA)
  P2 <- columnwisePercentage(M2, default = 0, digits = NA)
  
  all_correct <- function(x) all(almostEqual(colSums(x), rep(100, ncol(x))))
  
  expect_true(all_correct(P1))
  expect_true(all_correct(P2))
  
  expect_identical(dimnames(M1), dimnames(P1))
  expect_identical(dimnames(M2), dimnames(P2))
})
