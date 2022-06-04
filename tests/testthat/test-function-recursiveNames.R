test_that("recursiveNames() works", {

  expect_null(recursiveNames(1))  
  expect_null(recursiveNames(list(1, 2, 3)))

  L <- list(a = list(a_1 = NA, 
                     a_2 = NA),
            b = list(b_1 = NA, 
                     b_2 = list(b2_1 = NA)),
            c = NA)
  
  expect_identical(recursiveNames(L), c("$a", "$b", "$b$b_2"))
  expect_identical(recursiveNames(L, "L"), c("L$a", "L$b", "L$b$b_2"))
})
