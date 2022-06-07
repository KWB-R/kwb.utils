test_that("isNetworkPath() works", {

  f <- kwb.utils:::isNetworkPath

  expect_error(f())
  expect_false(f("abc"))
  expect_true(f("//abc"))
  expect_true(f("//abc/def/ghi"))
})
