test_that("user() works", {

  result <- kwb.utils:::user()
  
  expect_is(result, "character")
  expect_length(result, 1L)
  expect_true(result != "")

})
