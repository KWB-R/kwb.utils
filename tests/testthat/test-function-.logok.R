test_that(".logok() works", {

  f <- kwb.utils:::.logok

  expect_output(f(), "*** ok.", fixed = TRUE)

})
