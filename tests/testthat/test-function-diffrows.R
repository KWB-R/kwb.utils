test_that("diffrows() works", {

  x <- matrix(1:12, nrow = 3)

  d <- diffrows(x)

  expect_identical(nrow(d) + 1L, nrow(x))
  
  expect_equal(x[2, ] - x[1, ], d[1, ])
  expect_equal(x[3, ] - x[2, ], d[2, ])
  
  x <- matrix(1:4, ncol = 1, dimnames = list(NULL, "1a"))
  
  expect_identical(colnames(diffrows(x)), "1a")
})
