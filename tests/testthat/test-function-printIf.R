test_that("printIf() works", {
  
  df <- data.frame(a = 1:2, b = c("A", "B"))
  
  out_1 <- capture.output(printIf(FALSE, 1))
  out_2 <- capture.output(printIf(TRUE, 1))
  out_3 <- capture.output(printIf(TRUE, df))
  
  expect_identical(out_1, character())
  expect_true(all(out_2 != ""))
  expect_length(out_2, 2)
  
  expect_identical(out_3[1], "df:")
})
