test_that("guessSeparator() works", {

  x <- data.frame(key = LETTERS[1:3], value = 1:3)
  
  file_1 <- tempfile()
  file_2 <- tempfile()
  
  write.table(x, file_1, sep = ";")
  write.table(x, file_2, sep = ",")
  
  expect_identical(guessSeparator(file_1), ";")
  expect_identical(guessSeparator(file_2), ",")
})

test_that("getKeywordPositions() works", {
  
  word_matrix <- matrix(byrow = TRUE, nrow = 3, strsplit(split = "\\s+", paste(
    "Fest   gemauert in     der      Erden steht die",
    "Form   aus      Lehm   gebrannt Heute muss  die",
    "Glocke werden   frisch Gesellen seid  zur   Hand"
  ))[[1]])

  keywords <- c("frisch", "Erden", "Heute")
  
  pos <- getKeywordPositions(word_matrix, keywords)
  
  expect_true(is.data.frame(pos))
  expect_identical(dim(pos), c(2L, 3L))
  expect_identical(names(pos), keywords)
  expect_equal(pos$frisch, c(3, 3))
  expect_equal(pos$Erden, c(1, 5))
  expect_equal(pos$Heute, c(2, 5))
})
