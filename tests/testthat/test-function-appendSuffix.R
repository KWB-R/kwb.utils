test_that("appendSuffix() works", {
  
  values <- c("a", "b", "c")
  
  expect_equal(appendSuffix(values, ".1"), c("a.1", "b.1", "c.1"))
  
  expect_equal(
    appendSuffix(values, ".1", valuesToOmit = "c"), c("a.1", "b.1", "c")
  )
})
