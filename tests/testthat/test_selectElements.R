test_that("selectElements() works", {
  f <- selectElements
  L <- list(a = 1, b = "one")
  expect_error(f())
  expect_error(f(1))
  expect_equal(f(L, "a"), 1)
  expect_equal(f(L, "b"), "one")
})
