test_that("removeSpaces() works", {

  expect_identical(removeSpaces(" a b c "), "abc")
  expect_identical(removeSpaces(" a  b  c "), "abc")
})
