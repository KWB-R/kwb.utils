test_that("roundColumns() works", {
  
  x <- data.frame(a = c(1.23456, 234.5678), b = c("A", "B"), c = c(1245, 234))
  
  x1 <- roundColumns(x, columnNames = c("a", "c"), digits = 2)

  x2 <- roundColumns(x, digits = list(a = 1, c = -2))

  expect_equal(x1$a, round(x$a, 2))
  expect_equal(x1$c, round(x$c, 2))
  
  expect_equal(x2$a, round(x$a, 1))
  expect_equal(x2$c, round(x$c, -2))
}) 

test_that("matchesCriteria() works", {
  
  Data <- data.frame(A = c("x", "y", "z"), B = c( 1,   2,   3))
  
  criteria1 <- c("A %in% c('y', 'z')", "B %in% 1:3")
  criteria2 <- c("AB %in% c('y', 'z')", "B <- 1")
  
  expect_equal(matchesCriteria(Data, criteria1), c(F, T, T))
  expect_equal(matchesCriteria(Data, criteria1[1]), c(F, T, T))
  expect_equal(matchesCriteria(Data, criteria1[2]), c(T, T, T))
  
  expect_error(matchesCriteria(Data, criteria2))
  expect_error(matchesCriteria(Data, criteria2[1]))
  expect_error(matchesCriteria(Data, criteria2[2]))
  
  expect_true(all(matchesCriteria(Data)))
})
