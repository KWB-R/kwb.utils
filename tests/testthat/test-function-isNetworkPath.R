test_that("isNetworkPath() works", {

  f <- kwb.utils:::isNetworkPath

  expect_error(f())
  expect_false(.isNetworkPath("abc"))
  expect_true(.isNetworkPath("//abc"))
  expect_true(.isNetworkPath("//abc/def/ghi"))
})
