test_that("copyListElements() works", {
  
  x <- list(list(a = 1), list(b = 2), list(c = 3))
  y <- list("b1", "b2", "b3")
  
  expect_error(copyListElements("a", 1))
  expect_error(copyListElements(list(), 1))
  expect_error(copyListElements(list(list(a = 1)), list(b = 2, c = 3)))
  expect_error(copyListElements(list(list()), list(a = 1), name = character()))
  
  z <- copyListElements(x, y)
  
  expect_is(z, "list")
  expect_length(z, length(x))
})
