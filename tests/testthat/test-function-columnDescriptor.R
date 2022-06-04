test_that("columnDescriptor() works", {

  f <- kwb.utils:::columnDescriptor

  result <- f()

  expect_identical(names(result), c("match", "fixed"))
})
