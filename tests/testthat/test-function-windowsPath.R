test_that("windowsPath() and rStylePath() work", {

  x <- "a/b/c"
  y <- windowsPath(x)
  
  expect_identical(hsCountInStr("\\\\", y), hsCountInStr("/", x))
  expect_identical(rStylePath(y), x)
})
