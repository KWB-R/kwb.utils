
test_that(".OStype() returns 'unix' or 'windows'", {

  expect_true(.OStype() %in% c("unix", "windows"))
})

test_that(".isNetworkPath() works", {

  expect_false(.isNetworkPath("abc"))
  expect_true(.isNetworkPath("//abc"))
  expect_true(.isNetworkPath("//abc/def/ghi"))
})

test_that(".showCommand() works", {

  expect_error(.showCommand())
  expect_output(.showCommand("/path/to/program option1 option2 file"))
})
