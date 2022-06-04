test_that("clearConsole() works", {
  
  out <- capture.output(clearConsole())
  
  expect_true(out == "\014")
})
