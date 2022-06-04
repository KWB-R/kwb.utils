test_that("pairwise() works", {
  
  x <- c("a.1", "b_hi", "c", "a.2", "d", "b_bye")
  
  expected <- c("a.1", "a.2", "b_hi", "b_bye", "c", "d")
  
  expect_equal(pairwise(x, starts = c("a.", "b_")), expected)
  expect_equal(pairwise(x, split = "[._]"), expected)
  
  expected <- c("a.1", "b_hi", "b_bye", "c", "a.2", "d")
  expect_equal(pairwise(x, split = "_"), expected)
  expect_equal(pairwise(x), expected)
})
