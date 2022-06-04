test_that("startsToRanges() works", {
  
  starts <- c(1, 10, 20, 35)
  
  expect_identical(startsToRanges(starts, lastStop = 50), data.frame(
    from = c(2, 11, 21, 36),
    to = c(9, 19, 34, 50)
  ))
  
  expect_identical(
    startsToRanges(starts, lastStop = 55, startOffset = 2, stopOffset = 2),
    data.frame(
      from = c(3, 12, 22, 37),
      to = c(8, 18, 33, 55)
    )
  )
})
