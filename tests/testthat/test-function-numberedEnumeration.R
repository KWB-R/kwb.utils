test_that("numberedEnumeration() works", {

  f <- kwb.utils:::numberedEnumeration

  expect_error(f())
  
  y <- f(LETTERS[1:5])
  
  expect_type(y, "character")
})
