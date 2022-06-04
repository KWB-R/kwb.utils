test_that("removeElements() works", {
  
  x <- list(a = 1, b = 2:3, c = 3:5)
  
  y <- removeElements(x, elements = "a")
  
  expect_is(y, "list")
  expect_length(y, 2)
  expect_identical(y, x[c("b", "c")])
})
