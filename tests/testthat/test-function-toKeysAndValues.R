test_that("toKeysAndValues() works", {

  y <- toKeysAndValues("a=1,b=2,c=3")
  
  expect_true(is.list(y))
  expect_length(y, 2)
  expect_identical(names(y), c("keys", "values"))
  expect_identical(y$keys, c("a", "b", "c"))
  expect_identical(y$values, c("1", "2", "3"))
})
