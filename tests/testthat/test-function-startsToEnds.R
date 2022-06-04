test_that("startsToEnds() works", {
  
  starts <- c(1, 10, 20, 35)
  
  expect_identical(startsToEnds(starts, lastStop = 50), c(9, 19, 34, 50))
  
  expect_identical(
    startsToEnds(starts, lastStop = 50, stopOffset = 2), 
    c(8, 18, 33, 50)
  )
})
