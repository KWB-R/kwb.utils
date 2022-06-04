test_that(".log() works", {

  f <- kwb.utils:::.log

  expect_output(f(), "***", fixed = TRUE)

})
