test_that("countOrSum() works", {

  x <- data.frame(
    Group = rep(c("A", "B", "C"), 4),
    Even = rep(c(FALSE, TRUE), 6),
    Value = seq_len(12)
  )

  y_1 <- countOrSum(x, "Group")
  y_2 <- countOrSum(x, c("Group", "Even"))
  y_3 <- countOrSum(x, "Group", sum.up = "Value")
  y_4 <- countOrSum(x, c("Group", "Even"), sum.up = "Value")
  
  expect_true(all(y_1 == 4))
  expect_identical(dim(y_1), 3L)
  expect_identical(dim(y_2), c(3L, 2L))
  expect_identical(dim(y_3), 3L)
  expect_identical(dim(y_4), c(3L, 2L))
  
  n <- nrow(x)
  
  expect_identical(sum(y_1), n)
  expect_identical(sum(y_2), n)
  
  S <- sum(x$Value)

  expect_identical(sum(y_3), S)
  expect_identical(sum(y_4), S)
})

test_that("hsMovingMean() work", {

  x <- 1:10
  
  expect_error(hsMovingMean(x, n = 2))
  
  y_1 <- hsMovingMean(x, n = 1)
  y_3 <- hsMovingMean(x, n = 3)
  y_5 <- hsMovingMean(x, n = 5)
  
  expect_equal(y_1, x)
  expect_identical(sum(is.na(y_3)), 2L)
  expect_identical(sum(is.na(y_5)), 4L)
})

test_that("percentageOfMaximum() works", {

  values <- 1:10
  
  expect_equal(percentageOfMaximum(values) / 100 * max(values), values)
  
  expect_true(all(is.na(percentageOfMaximum(c(1:3, NA), na.rm = FALSE))))
})

test_that("percentageOfSum() works", {

  expect_identical(sum(percentageOfSum(1:10)), 100)
  
  expect_true(all(is.na(percentageOfSum(c(1:3, NA), na.rm = FALSE))))
})

test_that("percentage() works", {
  
  x <- 1:10
  
  expect_identical(percentage(x, 5), x / 5 * 100)
})

test_that("relativeCumulatedSum() works", {

  expect_identical(relativeCumulatedSum(1), 100)
  
  expect_identical(lastElement(relativeCumulatedSum(1:10)), 100)
  expect_identical(order(relativeCumulatedSum(1:5)), 1:5)
})

test_that("columnwisePercentage() works", {
  
  M1 <- matrix(sample(100, 12), nrow = 4, dimnames = list(LETTERS[1:4], 1:3))
  
  # Introduce some NA
  values <- as.numeric(M1)
  values[sample(length(values), 3)] <- NA
  
  M2 <- matrix(values, nrow = nrow(M1), dimnames = dimnames(M1))
  
  expect_true(all(colSums(columnwisePercentage(M1, digits = NA)) == 100))
  
  expect_true(all(colSums(columnwisePercentage(M2, digits = NA)) == 100))
  
  expect_true(sum(is.na(colSums(columnwisePercentage(M2, default = NA)))) > 0)
}) 

test_that("colStatistics() works", {

  x <- data.frame(a = 1:10, b = 2:11)
  y <- colStatistics(x)
  
  expect_identical(dim(y), c(2L, 6L))
  
  expect_equal(y$sum, c(sum(x$a), sum(x$b)))
  expect_equal(y$min, c(min(x$a), min(x$b)))
  expect_equal(y$max, c(max(x$a), max(x$b)))
  
  y <- colStatistics(x, functionColumn = TRUE)
  
  expect_true(! is.null(y$FUN))
  
  expect_error(colStatistics(x, functions = "Min"))
})

# colStatisticOneFunction ------------------------------------------------------
# colMinima --------------------------------------------------------------------
# colMaxima --------------------------------------------------------------------
# colNaNumbers -----------------------------------------------------------------
