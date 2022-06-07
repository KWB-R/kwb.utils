test_that("textToObject() works", {

  f <- kwb.utils:::textToObject

  expect_error(f())

  expect_identical(f("list(a = 1)"), list(a = 1))
})
