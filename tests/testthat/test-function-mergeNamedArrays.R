test_that("mergeNamedArrays() works", {
  
  a1 <- array(
    1:12, dim = c(2, 4, 2), dimnames = list(
      paste0("x", 1:2), paste0("y", 1:4), paste0("z", 1:2)
  ))

  a2 <- array(
    11:16, dim = c(1, 3, 2), dimnames = list(
      "x3", paste0("y", 2:4), paste0("z", 1:2)
  ))

  a <- mergeNamedArrays(list(a1, a2))
  
  dim_names_1 <- dimnames(a1)
  dim_names_2 <- dimnames(a2)
  dim_names <- dimnames(a)

  for (i in 1:3) {
    
    expect_true(all(dim_names_1[[i]] %in% dim_names[[i]]))
    expect_true(all(dim_names_2[[i]] %in% dim_names[[i]]))
  }
  
  expect_error(mergeNamedArrays(list(a1, unname(a2))))
})
