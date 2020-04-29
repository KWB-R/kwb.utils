test_that("assertFinalSlash() works", {

  f <- kwb.utils::assertFinalSlash

  expect_error(f())

  expect_true(all(grepl("/$", f(c("a", "b/")))))
})
