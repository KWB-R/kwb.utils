test_that("toColumnName() works", {

  f <- kwb.utils:::toColumnName

  expect_error(f())
  expect_identical(f(c("a b", "a.b")), rep("a_b", 2L))
})
