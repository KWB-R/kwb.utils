x <- c("a.1", "b_hi", "c", "a.2", "d", "b_bye")

test_that("the starts parameter is considered correctly", {

  expect_equal(pairwise(x, starts = c("a.", "b_")), 
               c("a.1", "a.2", "b_hi", "b_bye", "c", "d"))

})

test_that("the split parameter is considered correctly", {
  
  expect_equal(pairwise(x, split = "[._]"), 
               c("a.1", "a.2", "b_hi", "b_bye", "c", "d"))
  
  expect_equal(pairwise(x), 
               c("a.1", "b_hi", "b_bye", "c", "a.2", "d"))
  
  expect_equal(pairwise(x, split = "\\."), 
               c("a.1", "a.2", "b_hi", "c", "d", "b_bye"))
})
