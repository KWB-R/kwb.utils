test_that("csvTextToDataFrame() works", {
  
  y <- csvTextToDataFrame("a,b,c\n1,2,3", header = TRUE, sep = ",")
  
  expect_equal(dim(y), c(1, 3))
  expect_named(y)
  expect_equal(names(y), c("a", "b", "c"))
})
