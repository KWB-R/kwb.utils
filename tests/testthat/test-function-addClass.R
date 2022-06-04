test_that("addClass() works", {
  
  y_1 <- addClass(1, "a")
  y_2 <- addClass(1, "a", first = FALSE)
  
  expect_is(y_1, "a")
  expect_is(addClass(y_1, "b"), "b")
  
  expect_silent(addClass(1, "a"))
  expect_output(addClass(1, "a", dbg = TRUE))
  
  expect_identical(firstElement(class(y_1)), "a")
  expect_identical(lastElement(class(y_2)), "a")
})
