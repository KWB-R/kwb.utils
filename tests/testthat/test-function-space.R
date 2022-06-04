test_that("space() works", {

  f <- kwb.utils:::space

  expect_identical(f(), "  ")
  expect_identical(f(5, 1), "     ")
  expect_identical(f(2, 2), "    ")
  
})
