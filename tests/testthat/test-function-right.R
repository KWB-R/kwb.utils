test_that("right() works", {

  f <- kwb.utils:::right

  expect_error(f())

  expect_identical(f("abcdef", 3), "def")
})
