test_that("breakInSequence() works", {

  expect_error(breakInSequence("a"))
  expect_identical(breakInSequence(c(1, 2, 4, 5)), 2L)
  expect_identical(breakInSequence(c(1, 3, 5, 8, 10, 12), expectedDiff = 2), 3L)
})
