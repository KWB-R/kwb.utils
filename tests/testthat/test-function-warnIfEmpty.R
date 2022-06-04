test_that("warnIfEmpty() works", {

  expect_warning(warnIfEmpty(NULL))  
  expect_warning(warnIfEmpty(data.frame()))  
  expect_warning(warnIfEmpty(list()))
  expect_warning(warnIfEmpty(c()))
  
  expect_silent(data.frame(a = 1))
  expect_silent(list(a = 1))
  expect_silent(1)
})
