test_that("rangeToSequence() works", {

  f <- kwb.utils:::rangeToSequence

  expect_error(f())
  expect_error(f(1:3))
  expect_identical(f(1:2), 1:2)
  expect_identical(f(c(5, 1)), 5:1)
})
