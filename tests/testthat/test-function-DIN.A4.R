test_that("DIN.A4() works", {

  f <- kwb.utils:::DIN.A4

  result <- f()
  expect_identical(names(result), c("height.cm", "width.cm"))

})
