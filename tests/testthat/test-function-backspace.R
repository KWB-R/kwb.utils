test_that("backspace() works", {

  f <- kwb.utils:::backspace

  expect_identical(f(), "\b")
  expect_identical(f(3), "\b\b\b")
})
