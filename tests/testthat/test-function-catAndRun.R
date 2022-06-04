test_that("catAndRun() works", {
  
  x <- 1L
  
  expect_output(x <- catAndRun("work hard", x + 1L))
  
  expect_identical(x, 2L)
  
  expect_silent(catAndRun("work hard", x + 1L, dbg = FALSE))
})
