test_that("mySystemTime() works", {
  
  expect_output(y <- mySystemTime(max, list(1:10)))
  
  expect_is(y, "integer")
  expect_length(y, 1)
})
