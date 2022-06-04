test_that(".logstart() works", {

  f <- kwb.utils:::.logstart

  expect_output(f(TRUE, "Running"), "Running ...", fixed = TRUE)

})
