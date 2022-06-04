test_that("hsChrToNum() works", {

  expect_error(hsChrToNum("a", "de"))
  expect_error(hsChrToNum("1", "no_such_country"))
  expect_identical(hsChrToNum("1.0", "en"), 1.0)
  expect_identical(hsChrToNum("1,0", "de"), 1.0)
  expect_identical(hsChrToNum("1,000.0", "en"), 1000)
  expect_identical(hsChrToNum("1.000,0", "de"), 1000)
  expect_error(hsChrToNum("1,0000.0", "en"))
  expect_error(hsChrToNum("1.0000,0", "de"))

  x <- c("1.1", "2.2", "?")
  expect_warning(y <- hsChrToNum(x, "en", stopOnError = FALSE))
  expect_true(is.na(y[3]))
  expect_identical(attr(y, "errorIndices"), 3L)
})
