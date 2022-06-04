test_that("hsSubstSpecChars() works", {

  f <- kwb.utils:::hsSubstSpecChars

  expect_error(expect_warning(f(), "deprecated"))

})
