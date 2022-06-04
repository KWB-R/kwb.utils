
test_that("the log functions work", {

  expect_identical(capture.output(.logstart(FALSE, "x")), character())
  expect_identical(capture.output(.logok(FALSE)), character())
  
  text <- "Hello, world!"
  
  expect_true(grepl(text, capture.output(.logstart(TRUE, text))))
  expect_true(grepl("ok", capture.output(.logok(TRUE))))
  
  expect_length(capture.output({
    .log(text)
    .log("<EOM>")
  }), 1)
  
  expect_length(capture.output({
    .logline(text)
    .logline("<EOM>")
  }), 2)
})
