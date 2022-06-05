test_that("columnToDate() works", {

  f <- kwb.utils:::columnToDate

  expect_error(f())

  expect_output(result <- f(data.frame(a = "2022-06-05"), "a"))
  expect_identical(result$a, as.Date("2022-06-05"))
})
