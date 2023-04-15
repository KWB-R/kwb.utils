#kwb.utils::assignPackageObjects("kwb.utils")
#library(testthat)

test_that("catAndRun() works", {
  
  x <- 1L
  
  expect_output(
    x <- catAndRun("work hard", x + 1L), 
    regexp = "work hard \\.\\.\\. ok\\. \\(0\\.\\d\\d secs\\)", 
  )
  
  expect_identical(x, 2L)
  
  expect_silent(catAndRun("work hard", x + 1L, dbg = FALSE))
})

catAndRun("hello", Sys.sleep(time = 60))
