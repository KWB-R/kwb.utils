test_that("combineAlternatingly() works", {

  f <- kwb.utils:::combineAlternatingly

  expect_error(f())
  expect_identical(f(1:3, 11:13), as.integer(c(1, 11, 2, 12, 3, 13)))
})
