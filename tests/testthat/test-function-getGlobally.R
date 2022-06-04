test_that("getGlobally() works", {
  
  assign("a", 1, envir = .GlobalEnv)
  
  expect_true("a" %in% ls(envir = .GlobalEnv))
  
  expect_identical(getGlobally("a"), 1)
  
  expect_null(getGlobally("no_such_object"))
})
