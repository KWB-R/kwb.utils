test_that("createMatrix() works", {

  m1 <- createMatrix(c("A", "B", "C", "D", "E"), c("x", "y", "z"), -1)

  m2 <- createMatrix(c("A", "B", "C"))

  m3 <- createMatrix(c("A", "B", "C"), value = NA)

  m4 <- createMatrix(c("A", "B", "C"), name.row = "Letters")

  expect_true(all(sapply(list(m1, m2, m3, m4), is.matrix)))
  
  expect_true(all(m1 == -1))
  expect_true(all(m2 == 0))
  expect_true(all(is.na(m3)))
  expect_identical(names(dimnames(m4))[1], "Letters")
  
  expect_identical(colnames(m1), c("x", "y", "z"))
  expect_identical(rownames(m1), LETTERS[1:5])
  
  expect_identical(rownames(m2), LETTERS[1:3])
  expect_identical(rownames(m3), LETTERS[1:3])
  expect_identical(rownames(m4), LETTERS[1:3])
})
