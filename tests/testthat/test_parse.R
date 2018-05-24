test_that("guessSeparator() works", {

  x <- data.frame(key = LETTERS[1:3], value = 1:3)
  
  file_1 <- tempfile()
  file_2 <- tempfile()
  file_3 <- tempfile()
  
  write.table(x, file_1, sep = ";")
  write.table(x, file_2, sep = ",")
  writeLines(c("a,b;c", "1;2,3"), file_3)
  
  expect_identical(guessSeparator(file_1), ";")
  expect_identical(guessSeparator(file_2), ",")

  expect_identical(guessSeparator(c(file_1, file_2)), c(";", ","))
  
  guessSeparator(file_3)
  guessSeparator(file_3, separators = c(" ", "+"))
})

test_that("getKeywordPositions() works", {
  
  word_matrix <- matrix(byrow = TRUE, nrow = 3, strsplit(split = "\\s+", paste(
    "Fest   gemauert in     der      Erden steht die",
    "Form   aus      Lehm   gebrannt Heute muss  die",
    "Glocke werden   frisch Gesellen seid  zur   Hand"
  ))[[1]])

  keywords <- c("frisch", "Erden", "Heute")
  
  pos_dframe <- getKeywordPositions(word_matrix, keywords)
  pos_matrix <- getKeywordPositions(word_matrix, keywords, asDataFrame = FALSE)
  
  expect_is(pos_dframe, "data.frame")
  expect_is(pos_matrix, "matrix")
  
  expect_identical(dim(pos_dframe), c(2L, 3L))
  expect_identical(dim(pos_matrix), c(2L, 3L))
  
  expect_identical(colnames(pos_dframe), keywords)
  expect_identical(colnames(pos_matrix), keywords)
  
  expect_equal(pos_dframe$frisch, c(3, 3))
  expect_equal(pos_dframe$Erden, c(1, 5))
  expect_equal(pos_dframe$Heute, c(2, 5))
  
  expect_equal(pos_matrix[, "frisch"], c(3, 3))
  expect_equal(pos_matrix[, "Erden"], c(1, 5))
  expect_equal(pos_matrix[, "Heute"], c(2, 5))
  
  expect_error(getKeywordPositions(word_matrix, "no_such_word"))
})
