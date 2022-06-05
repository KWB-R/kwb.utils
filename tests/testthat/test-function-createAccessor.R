test_that("createAccessor() works", {

  f <- kwb.utils:::createAccessor

  expect_error(f())

  x1 <- list(a = 1, b = 2, c = 3)
  x2 <- data.frame(a = 1, b = 2, c = 3)
  
  a1 <- f(x1)
  a2 <- f(x2)
  
  expect_is(a1, "function")
  expect_is(a2, "function")

  expect_identical(a1("a"), x1[["a"]])
  expect_identical(a1(c("a", "b")), x1[c("a", "b")])
  
  expect_identical(a2("c"), x2[["c"]])
  expect_identical(a2(c("a", "c")), x2[c("a", "c")])
})
