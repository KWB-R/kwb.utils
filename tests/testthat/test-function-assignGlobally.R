test_that("assignGlobally() works", {

  f <- assignGlobally

  expect_error(expect_warning(f()))

})
