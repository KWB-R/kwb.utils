test_that("lastElement() works", {

  f <- kwb.utils:::lastElement

  expect_error(f())

  expect_identical(f(list(a = 1, b = 2)), 2)
  
  expect_identical(
    resetRowNames(f(data.frame(a = 1:2, b = 2:3))), 
    data.frame(a = 2L, b = 3L)
  )
  
  expect_identical(f(c("a", "b")), "b")
})
