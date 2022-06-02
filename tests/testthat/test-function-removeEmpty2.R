test_that("removeEmpty2() works", {

  f <- kwb.utils::removeEmpty2

  expect_error(f())

  expect_identical(f(c("a", "", "b")), c("a", "b"))
  expect_identical(f(c("a", "b", "c")), c("a", "b", "c"))
})
