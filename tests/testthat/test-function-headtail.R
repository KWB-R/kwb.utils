test_that("headtail() works", {
  
  x <- data.frame(number = 1:26, letter = LETTERS)
  
  expect_error(headtail(1))
  expect_error(headtail(list(a = 1, b = 2)))
               
  out_1 <- capture.output(headtail(x))
  out_2 <- capture.output(headtail(x, 10))
  out_3 <- capture.output(headtail(x, 16))
  out_4 <- capture.output(headtail(x[10:19, ], 10))
  
  expect_length(out_1,  6 + 3)
  expect_length(out_2, 10 + 3)
  expect_length(out_3, 16 + 3)
  expect_length(out_4, 10 + 1)

  for (out in list(out_1, out_2, out_3)) {
  
    expect_length(grep("rows omitted", out), 1)
  }
})
