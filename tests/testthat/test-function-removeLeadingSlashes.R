test_that("remove_leading_slashes() works", {

  expect_error(removeLeadingSlashes())
  expect_identical(removeLeadingSlashes("//a"), "a")
  expect_identical(removeLeadingSlashes("/a"), "a")
  expect_identical(removeLeadingSlashes("a"), "a")
  
})
