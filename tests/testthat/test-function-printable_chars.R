test_that("printable_chars() works", {

  f <- kwb.utils:::printable_chars

  result <- f()

  expect_true(all(LETTERS %in% result))
})
