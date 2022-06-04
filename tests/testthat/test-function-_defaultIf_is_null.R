test_that(".defaultIf(is.null, x, default) returns default for x = NULL", {
  expect_equal(.defaultIf(is.null, NULL, 10), 10)
  expect_equal(.defaultIf(is.null, NULL, "abc"), "abc")
})
