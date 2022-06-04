test_that("assignPackageObjects() works", {

  assignPackageObjects("kwb.utils")
  
  expect_true("assignPackageObjects" %in% ls(envir = .GlobalEnv))
})
