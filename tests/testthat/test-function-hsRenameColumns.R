test_that("hsRenameColumns() works", {

  f <- kwb.utils:::hsRenameColumns

  expect_error(expect_warning(f(), "deprecated"))

})
