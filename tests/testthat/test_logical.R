test_that("almostEqual() works", {
  
  expect_error(almostEqual(1, 1:2))
  expect_error(almostEqual(1:2, 3))
  
  expect_false(almostEqual(1, 2))
  expect_true(almostEqual(1, 1))
  
  expect_false(almostEqual(1, 1.1, 0.1))
  expect_true(almostEqual(1, 1.09, 0.1))
})

test_that("allAreEqual() works", {
  
  x_1 <- c(1, 1, 1)
  x_2 <- c(1, 1, 2)
  
  expect_true(allAreEqual(x_1))
  expect_false(allAreEqual(x_2))
  
  expect_true(allAreEqual(x_1, method = 2))
  expect_false(allAreEqual(x_2, method = 2))
})

test_that("isNaInAllColumns() and isNaInAllRows() work", {
  
  x_1 <- data.frame(a = 1:2, b = NA)
  x_2 <- rbind(x_1, data.frame(a = NA, b = NA))
  
  expect_identical(isNaInAllColumns(x_1), c(FALSE, FALSE))
  expect_identical(isNaInAllColumns(x_2), c(FALSE, FALSE, TRUE))
  
  expect_identical(isNaInAllRows(x_1), c(FALSE, TRUE))
  expect_identical(isNaInAllRows(x_2), c(FALSE, TRUE))
  
  expect_error(isNaInAllColumns(1))
  expect_error(isNaInAllRows(1))
})

test_that("allAreIdentical() works", {
  
  expect_error(allAreIdentical(1))
  expect_error(allAreIdentical(list()))
  
  expect_message(allAreIdentical(list(1)))
  
  expect_true(allAreIdentical(list(1, 1)))
  expect_false(allAreIdentical(list(1, 2)))
})

test_that("matchesCriteria() works", {
  
  Data <- data.frame(A = c("x", "y", "z", NA), B = c(1, 2, NA, 4))
  
  criteria_1 <- c("A %in% c('y', 'z')", "B %% 2 == 0")
  
  expect_warning(y <- matchesCriteria(Data, criteria_1))
  expect_identical(y, c(FALSE, TRUE, NA, FALSE))
  
  y <- matchesCriteria(Data, criteria_1[1])
  expect_identical(y, c(FALSE, TRUE, TRUE, FALSE))
  
  expect_warning(y <- matchesCriteria(Data, criteria_1[2]))
  expect_identical(y, c(FALSE, TRUE, NA, TRUE))

  expect_error(matchesCriteria(Data, "AB %in% c('y', 'z')"))

  expect_true(all(matchesCriteria(Data)))
  
  expect_message(y <- matchesCriteria(
    Data, criteria_1, add.details = TRUE, na.to.false = TRUE
  ))

  expect_identical(removeAttributes(y), c(FALSE, TRUE, FALSE, FALSE))
  
  details <- attr(y, "details")
  
  expect_true(! is.null(criteria <- attr(details, "criteria")))
  
  expect_identical(as.character(criteria$condition), criteria_1)
  
  expect_error(matchesCriteria(Data, "sum(B, na.rm = TRUE)"))
})
