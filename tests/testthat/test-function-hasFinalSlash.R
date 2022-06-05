test_that("hasFinalSlash() works", {

  f <- kwb.utils:::hasFinalSlash

  expect_error(f())
  expect_true(f("a/"))
  expect_false(f("a"))
  expect_identical(f(c("a/", "b")), c(TRUE, FALSE))
})
