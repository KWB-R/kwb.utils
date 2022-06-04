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
