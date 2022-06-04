test_that("splitAlongDim() works", {
  
  expect_error(splitAlongDim(list(), 1))
  
  # Define an array
  A <- array(1:8, dim = c(2, 2, 2), dimnames = list(
    paste0("x", 1:2), paste0("y", 1:2), paste0("z", 1:2)
  ))

  expect_error(splitAlongDim(A, 4))
  
  splitAlongDim(A, 1)
  splitAlongDim(A, 2)
  splitAlongDim(A, 3)
})
