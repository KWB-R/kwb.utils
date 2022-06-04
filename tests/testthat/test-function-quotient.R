test_that("quotient() works", {
  
  expect_equal(quotient(1, 2), 0.5)
  expect_warning(quotient(1, 0))
  expect_silent(y <- quotient(1, 0, warn = FALSE))
  expect_identical(y, Inf)
  expect_identical(quotient(1, 0, substitute.value = 999, warn = FALSE), 999)
})
