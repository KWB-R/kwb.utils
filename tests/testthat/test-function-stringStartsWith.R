test_that("stringStartsWith() works", {

  expect_identical(
    stringStartsWith(c("abc", "Kabeljau", "Arabella"), "ab"),
    c(TRUE, FALSE, FALSE)
  )
  
  expect_identical(
    stringStartsWith(c("abc", "Kabeljau", "Arabella"), "A"),
    c(FALSE, FALSE, TRUE)
  )
})
