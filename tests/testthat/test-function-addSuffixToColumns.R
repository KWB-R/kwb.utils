test_that("addSuffixToColumns() works", {

  f <- kwb.utils:::addSuffixToColumns

  expect_error(f())

  data <- data.frame(a = 1:3, b = 2:4)
  
  result1 <- f(data, "_old")
  result2 <- f(data, "_old", except = "b")
  
  expect_identical(names(result1), c("a_old", "b_old"))
  expect_identical(names(result2), c("a_old", "b"))
})
