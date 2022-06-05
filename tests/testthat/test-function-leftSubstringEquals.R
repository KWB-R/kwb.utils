test_that("leftSubstringEquals() works", {

  f <- kwb.utils:::leftSubstringEquals

  expect_error(f())
  expect_true(f("abcde", "abc"))
  expect_identical(f(c("abcde", "abdde"), "abc"), c(TRUE, FALSE))
})
