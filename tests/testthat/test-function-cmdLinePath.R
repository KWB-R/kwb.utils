test_that("cmdLinePath() works", {

  expect_error(cmdLinePath())
  
  y <- cmdLinePath("x/y/z")
  
  expect_is(y, "character")
  
  expect_true(grepl('^".*"$', y))
})
