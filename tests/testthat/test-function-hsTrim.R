test_that("hsTrim() works", {

  expect_identical(hsTrim("  a "), "a")
  expect_identical(hsTrim("  a    b "), "a b")
  expect_identical(hsTrim(" \ta \t b \t"), "a b")
})
