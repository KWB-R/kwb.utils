
test_that(".OStype() returns 'unix' or 'windows'", {

  expect_true(.OStype() %in% c("unix", "windows"))
})

test_that(".isNetworkPath() works", {

  expect_false(.isNetworkPath("abc"))
  expect_true(.isNetworkPath("//abc"))
  expect_true(.isNetworkPath("//abc/def/ghi"))
})
