test_that("orderBy() works", {

  f <- kwb.utils:::orderBy

  expect_error(f())
  expect_error(f(df, "x"))

  df <- data.frame(a = 1:3, b = 3:1)
  
  expect_identical(f(df, by = "a"), df)
  expect_identical(f(df, by = "b"), resetRowNames(df[3:1, ]))
  
  expect_identical(
    f(df, by = "a", decreasing = TRUE), 
    resetRowNames(df[3:1, ])
  )
  
})
