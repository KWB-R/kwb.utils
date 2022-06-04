test_that("argsCsv() works", {

  f <- kwb.utils:::argsCsv

  result <- f()

  expect_identical(names(result), c("sep", "dec"))
})
