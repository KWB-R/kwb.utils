test_that("hsMatrixToListForm() works", {

  df <- data.frame(a = 1:3, b = 11:13, c = 21:23)
  
  y <- hsMatrixToListForm(
    df, keyFields = "a", colNamePar = "par", colNameVal = "val"
  )
  
  expect_is(y, "data.frame")
  
  expect_identical(nrow(y), 6L)
  
  expect_identical(names(y), c("a", "par", "val"))
  
  x <- reshape(y, direction = "wide", timevar = "par", idvar = "a")

  expect_equal(
    structure(df, names = NULL), 
    structure(x, names = NULL, reshapeWide = NULL)
  )
})
