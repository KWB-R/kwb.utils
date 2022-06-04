test_that("stringContains() works", {

  expect_true(all(stringContains(c("abc", "Kabeljau", "Arabella"), "ab")))
  
  expect_identical(stringContains(c("abc", "Kabeljau", "Arabella"), "abc"), 
                   c(TRUE, FALSE, FALSE))
})
