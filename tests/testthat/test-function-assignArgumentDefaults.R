test_that("assignArgumentDefaults() works", {

  f <- kwb.utils::assignArgumentDefaults

  expect_error(f())
  
  f(kwb.utils::collapsed)
  
  expect_true("collapse" %in% ls(envir = .GlobalEnv))
})
