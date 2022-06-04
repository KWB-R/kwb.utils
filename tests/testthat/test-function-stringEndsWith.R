test_that("stringEndsWith() works", {

  expect_identical(
    stringEndsWith(c("abc", "Kabeljau", "Arabella"), "a"),
    c(FALSE, FALSE, TRUE)
  )
  
  expect_identical(
    stringEndsWith(c("abc", "Kabeljau", "Arabella"), "jau"),
    c(FALSE, TRUE, FALSE)
  )
})
