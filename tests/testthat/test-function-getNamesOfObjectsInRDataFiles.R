test_that("getNamesOfObjectsInRDataFiles() works", {

  f <- kwb.utils:::getNamesOfObjectsInRDataFiles

  expect_error(expect_warning(f(), "deprecated"))

})
