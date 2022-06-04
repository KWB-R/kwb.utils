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
