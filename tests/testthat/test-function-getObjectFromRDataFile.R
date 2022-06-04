test_that("getObjectFromRDataFile() works", {

  f <- kwb.utils:::getObjectFromRDataFile

  expect_error(expect_warning(f(), "deprecated"))

})
