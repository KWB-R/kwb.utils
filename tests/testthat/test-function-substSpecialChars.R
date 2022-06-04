test_that("substSpecialChars() works", {
  
  expect_identical(substSpecialChars("% ?"), "proz")
})
