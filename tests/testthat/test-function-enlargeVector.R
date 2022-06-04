test_that("enlargeVector() works", {

  expect_error(enlargeVector(1:10, 5))
  
  v1 <- enlargeVector(1:5, 10, fill.with = 0) 
  v2 <- enlargeVector(1:5, 10, fill.with = NA) 
  v3 <- enlargeVector(c("a", "b", "c"), 10) 
  v4 <- enlargeVector(c("a", "b", "c"), 10, fill.with = "?")

  expect_length(v1, 10)
  expect_length(v2, 10)
  expect_length(v3, 10)
  expect_length(v4, 10)

  expect_equal(sum(v1 == 0), 5L)
  expect_equal(sum(is.na(v2)), 5L)  
  expect_equal(sum(v3 == ""), 7L)
  expect_equal(sum(v4 == "?"), 7L)
})
