test_that("isLoaded() and setLoaded() work as expected", {
  
  name <- "abc"
  
  expect_false(isLoaded(name))
  
  setLoaded(name)
  
  expect_true(isLoaded(name))
})
