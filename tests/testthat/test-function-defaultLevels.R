test_that("defaultLevels() works", {
  
  expect_equal(defaultLevels(c(1, 3, 4, 5, 4)), 1:5)
  
  expect_equal(
    defaultLevels(c(1920, 1950, 1970), step = 10), seq(1920, 1970, 10)
  )
})
