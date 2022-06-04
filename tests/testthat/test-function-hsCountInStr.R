test_that("hsCountInStr() works", {
  
  x <- "Nananananananana"
  
  expect_identical(hsCountInStr("a", x), 8L)
  expect_identical(hsCountInStr("na", x), 7L)
  expect_identical(hsCountInStr("nanana", x), 2L)
  expect_identical(hsCountInStr("(na){3}", x), 2L)
})
