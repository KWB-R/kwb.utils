test_that("guessSeparator() works", {

  x <- data.frame(key = LETTERS[1:3], value = 1:3)
  
  file_1 <- tempfile()
  file_2 <- tempfile()
  file_3 <- tempfile()
  
  write.csv(x, file_1)
  write.csv2(x, file_2)
  writeLines(c("a,b;c", "", "1;2,3"), file_3)
  
  expect_identical(guessSeparator(file_1), ",")
  expect_identical(guessSeparator(file_2), ";")

  expect_identical(guessSeparator(c(file_1, file_2)), c(",", ";"))
  
  expect_identical(guessSeparator(file_3, separators = ","), ",")
  
  expect_warning(guessSeparator(file_3, separators = c(",", ";")))
  expect_warning(guessSeparator(file_3, separators = "@"))
})
