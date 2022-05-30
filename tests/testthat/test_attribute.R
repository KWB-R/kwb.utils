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

test_that("hsRestoreAttributes() works", {
  
  x <- structure(1, a = 2, b = 3)
  
  y <- hsRestoreAttributes(c(0, x), attributes(x))

  expect_identical(attributes(y), attributes(x))
})

test_that("getAttribute() works", {

  x <- structure(1, a = 2)
  
  expect_error(getAttribute(x, "b"))
  expect_silent(getAttribute(x, "a"))
  expect_identical(getAttribute(x, "a"), attr(x, "a"))
  
  y <- structure(1, a = structure(2, b = 3))
  
  expect_identical(kwb.utils::getAttribute(y, "a/b"), 3)
  expect_identical(kwb.utils::getAttribute(y, "a"), structure(2, b = 3))
  expect_error(kwb.utils::getAttribute(y, "a/b/c"))
  
  z <- structure(4, y = y)
  
  expect_identical(kwb.utils::getAttribute(z, "y/a/b"), 3)
})

test_that("removeAttributes() works", {

  x <- structure(1, a = 1, b = 2)  
  
  expect_null(attributes(removeAttributes(x)))
  expect_identical(attributes(removeAttributes(x, "a")), list(b = 2))
})
