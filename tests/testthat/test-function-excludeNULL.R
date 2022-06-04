test_that("excludeNULL() works", {

  L <- list(a = 1, b = NULL, c = "three")

  expect_error(excludeNULL(0))
  expect_identical(sum(sapply(excludeNULL(L, dbg = FALSE), is.null)), 0L)
})
