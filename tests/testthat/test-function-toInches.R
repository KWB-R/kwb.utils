test_that("toInches() works", {

  expect_equal(toInches(2.54), 1)
  expect_equal(toInches(25.4), 10)
})
