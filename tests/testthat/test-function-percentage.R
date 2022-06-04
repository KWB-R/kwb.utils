test_that("percentage() works", {
  
  x <- 1:10
  
  expect_identical(percentage(x, 5), x / 5 * 100)
})
