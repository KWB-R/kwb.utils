test_that("showPackageObjects() works", {

  f <- kwb.utils:::showPackageObjects

  expect_output(f())
  expect_silent(f(show = FALSE))

})
