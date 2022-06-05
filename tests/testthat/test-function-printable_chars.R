test_that("printable_chars() works", {

  f <- kwb.utils:::printable_chars

  result0 <- f()
  result1 <- f(level = 1)
  result2 <- f(level = 2)
  result3 <- f(level = 3)
  expect_error(f(level = 4))
  
  expect_identical(result0, result1)
  expect_true(length(result3) > length(result2))
  expect_true(length(result2) > length(result1))
  
  expect_true(all(LETTERS %in% result1))
  expect_true(!"a" %in% result1 && "a" %in% result2)
  expect_true(!"[" %in% result2 && "[" %in% result3)
})
