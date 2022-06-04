test_that("catLines() works", {
  
  text <- c("line 1", "line 2", "line 3")
  
  out <- capture.output(catLines(text))
  
  expect_identical(out, text)
})
