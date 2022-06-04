test_that("randomMatrix() works", {
  
  expect_true(is.matrix(randomMatrix()))
  
  dimension <- c(5L, 3L)
  
  expect_identical(dim(randomMatrix(dim = dimension)), dimension)
  
  values <- c(0, 0.5, 1, NA)
  
  expect_true(all(randomMatrix(dim = c(5, 3), values = values) %in% values))
})
