test_that("removeAttributes() works", {

  x <- structure(1, a = 1, b = 2)  
  
  expect_null(attributes(removeAttributes(x)))
  expect_identical(attributes(removeAttributes(x, "a")), list(b = 2))
})
