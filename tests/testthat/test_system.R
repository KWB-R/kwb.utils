
test_that(".OStype() returns 'unix' or 'windows'", {

  expect_true(.OStype() %in% c("unix", "windows"))
})
