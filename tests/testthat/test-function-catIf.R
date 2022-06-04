test_that("catIf() works", {
  
  text <- "Hello, world!"
  
  out_1 <- capture.output(catIf(FALSE, text))
  out_2 <- capture.output(catIf(TRUE, text))
  
  expect_identical(out_1, character())
  expect_identical(out_2, text)
})
