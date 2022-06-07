test_that("splitIntoLines() works", {

  f <- kwb.utils:::splitIntoLines

  expect_error(f())
  expect_error(f(1))
  expect_error(f(c("abc", "def")))
  
  expect_identical(f("a\nb"), c("a", "b"))
})
