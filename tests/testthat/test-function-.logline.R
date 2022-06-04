test_that(".logline() works", {

  f <- kwb.utils:::.logline

  expect_output(f(), "***", fixed = TRUE)

})
