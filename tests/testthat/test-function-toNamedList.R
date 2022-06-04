test_that("toNamedList() works", {
  
  x_vector <- c("Peter", "Paul", "Mary")
  
  x_list <- toNamedList(x_vector)
  
  expect_true(all(names(x_list) == x_vector))
})
