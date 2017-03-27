test_that(".defaultIf(is.null, x, default) returns default for x = NULL", {
  expect_equal(.defaultIf(is.null, NULL, 10), 10)
  expect_equal(.defaultIf(is.null, NULL, "abc"), "abc")
})

test_that(".defaultIf() works with a vector of length > 1", {
  expect_equal(.defaultIf(is.na, c(1, NA, 3), 2), 1:3)
  expect_equal(.defaultIf(isOddNumber, 1:5, 77), c(77, 2, 77, 4, 77))
})

test_that(".defaultIf() returns the count of replaced values", {
  out <- .defaultIf(is.na, c(1, NA, 3), 2, count = TRUE)
  expect_equal(attr(out, "count"), 1)
})

test_that(".defaultIf() does not return count by default", {
  out <- .defaultIf(is.na, c(1, NA, 3), 2)
  expect_null(attr(out, "count"))
})
