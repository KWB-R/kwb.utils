test_that("intToNumeralSystem works()", {

  result <- intToNumeralSystem(1:8, base = 2)
  
  expect_identical(nrow(result), 8L)
  expect_identical(rownames(result), as.character(1:8))
  expect_identical(colnames(result), as.character(rev(2^(0:3))))
})
