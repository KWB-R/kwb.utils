test_that("dropDim() works", {
  
  a <- array(1:8, dim = c(2, 2, 2), dimnames = list(
    paste0("x", 1:2), paste0("y", 1:2), paste0("z", 1:2)
  ))
  
  a1 <- dropDim(a[, 1, 1, drop = FALSE], dimension = 3)

  expect_identical(dim(a1), c(2L, 1L))
})
