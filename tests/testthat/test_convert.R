test_that("toFactor() works", {
  
  x <- c("b", "c", "a")
  
  y <- toFactor(x)
  
  expect_identical(levels(y), x)
  
  expect_identical(toFactor(y), y)
})

test_that("toPositiveIndices() works", {
  
  expect_identical(toPositiveIndices(c(-1, -2), n = 10), c(10, 9))
  expect_identical(toPositiveIndices(c(1, -1), n = 10), c(1, 10))
})

test_that("toInches() works", {

  expect_equal(toInches(2.54), 1)
  expect_equal(toInches(25.4), 10)
})

test_that("limitToRange() works", {
  
  expect_identical(range(limitToRange(1:20, left = 5, right = 15)), c(5, 15))
  expect_identical(range(limitToRange(1:20, left = -10, right = 50)), c(1, 20))
  
  expect_error(limitToRange(1:10, left = 1:2, right = 5))
})

test_that("toKeysAndValues() works", {

  y <- toKeysAndValues("a=1,b=2,c=3")
  
  expect_true(is.list(y))
  expect_length(y, 2)
  expect_identical(names(y), c("keys", "values"))
  expect_identical(y$keys, c("a", "b", "c"))
  expect_identical(y$values, c("1", "2", "3"))
})

test_that("underscoreToPercent() works", {
  
  expect_identical(underscoreToPercent("_"), "%")
  expect_identical(underscoreToPercent("_Y_m_d"), "%Y%m%d")
})

test_that("toFormula() works", {

  f1 <- toFormula("y", c("x1", "x2"))
  f2 <- toFormula("y", paste0("x", 1:20))
  f3 <- toFormula("BMI", c("height", "mass"), as.formula = FALSE)

  expect_s3_class(f1, "formula")
  expect_s3_class(f2, "formula")
  expect_true(is.character(f3))
})

test_that("frenchToAscii() works", {
  
  y <- frenchToAscii()
  
  expect_true(is.list(y))
  
  expect_true(all(grepl("^\\\\x[0-9a-fA-F]{2}", names(y))))
})

test_that("revertListAssignments() works", {
  
  abbreviation <- list(de = "Germany", en = "England")

  y <- revertListAssignments(abbreviation)

  expect_true(is.list(y))
  
  expect_identical(names(y), as.character(abbreviation))
  expect_identical(as.character(y), names(abbreviation))
  
  expect_identical(
    abbreviation,
    revertListAssignments(revertListAssignments(abbreviation))
  )
})

test_that("hsChrToNum() works", {

  expect_error(hsChrToNum("a", "de"))
  expect_error(hsChrToNum("1", "no_such_country"))
  expect_identical(hsChrToNum("1.0", "en"), 1.0)
  expect_identical(hsChrToNum("1,0", "de"), 1.0)
  expect_identical(hsChrToNum("1,000.0", "en"), 1000)
  expect_identical(hsChrToNum("1.000,0", "de"), 1000)
  expect_error(hsChrToNum("1,0000.0", "en"))
  expect_error(hsChrToNum("1.0000,0", "de"))

  x <- c("1.1", "2.2", "?")
  expect_warning(y <- hsChrToNum(x, "en", stopOnError = FALSE))
  expect_true(is.na(y[3]))
  expect_identical(attr(y, "errorIndices"), 3L)
})

test_that("hsValidValue() works", {
  
  expect_false(hsValidValue("1.1", "de"))
  expect_false(hsValidValue("1,1", "en"))
  
  expect_true(hsValidValue("1.1", "en"))
  expect_true(hsValidValue("1,1", "de"))
  
  x <- c("1.1", "2,2", "3.3", NA)
  
  expect_identical(hsValidValue(x, "en"), c(TRUE, FALSE, TRUE, TRUE))
  
  expect_identical(
    hsValidValue(x, "en", accept.na = FALSE), c(TRUE, FALSE, TRUE, FALSE)
  )
                   
  expect_output(hsValidValue(x, "de", dbg = TRUE))
})

test_that("hsStringToDouble() works", {
  
  expect_identical(hsStringToDouble("1.1"), 1.1)
  expect_warning(hsStringToDouble("1,1"))
  expect_identical(hsStringToDouble("1,1", dec = ","), 1.1)
  expect_error(hsStringToDouble("1.1.1"))
  
  expect_error(hsStringToDouble("1.1", dec = "*"))
  
  expect_warning(hsStringToDouble("1.1", dec = ","))
})

test_that("hsStringToDate() works", {

  expect_s3_class(hsStringToDate("2018-05-22"), "Date")
  expect_s3_class(hsStringToDate("22.05.2018", "%d.%m.%Y"), "Date")
  
  expect_error(hsStringToDate("22.05.2018"))
  
  expect_error(hsStringToDate(NA, "Y%-%m-%d"))
})
