test_that("randomValuesWithSum() works", {
  
  y <- randomValuesWithSum(10, 100)
  
  expect_equal(sum(y), 100)
  expect_named(y)
  expect_identical(names(y), as.character(seq_len(10)))
}) 

test_that("callWithStringsAsFactors() works", {

  get_stringsAsFactors <- function() getOption("stringsAsFactors")
  
  option_bak <- get_stringsAsFactors()

  d1 <- callWithStringsAsFactors(TRUE, get_stringsAsFactors)
  expect_identical(d1, TRUE)
  expect_identical(option_bak, get_stringsAsFactors())
  
  d2 <- callWithStringsAsFactors(FALSE, get_stringsAsFactors)
  expect_identical(d2, FALSE)
  expect_identical(option_bak, get_stringsAsFactors())

  expect_error(callWithStringsAsFactors(1, data.frame, a = "x"))
})

test_that("parallelNonNA() works", {
  
  expect_identical(parallelNonNA(c(1, NA, 3), c(NA, 2, NA)), c("1", "2", "3"))

  expect_identical(parallelNonNA(c(1, NA, NA), c(NA, 2, NA)), c("1", "2", ""))

  expect_warning(y <- parallelNonNA(c(1, 2, 3), c(1, 2, 4)))

  expect_equal(attr(y, "invalid"), data.frame(index = 3, a = 3, b = 4))
})

test_that("makeUnique() works", {

  x <- c("a", "a")
  
  expect_warning(y <- makeUnique(x))
  
  expect_true(! any(duplicated(y)))
  
  expect_silent(makeUnique(x, warn = FALSE))
})

test_that("recursiveNames() works", {

  expect_null(recursiveNames(1))  
  expect_null(recursiveNames(list(1, 2, 3)))

  L <- list(a = list(a_1 = NA, 
                     a_2 = NA),
            b = list(b_1 = NA, 
                     b_2 = list(b2_1 = NA)),
            c = NA)
  
  expect_identical(recursiveNames(L), c("$a", "$b", "$b$b_2"))
  expect_identical(recursiveNames(L, "L"), c("L$a", "L$b", "L$b$b_2"))
})

test_that("quotient() works", {
  
  expect_equal(quotient(1, 2), 0.5)
  expect_warning(quotient(1, 0))
  expect_silent(y <- quotient(1, 0, warn = FALSE))
  expect_identical(y, Inf)
  expect_identical(quotient(1, 0, substitute.value = 999, warn = FALSE), 999)
}) 

test_that("getOddNumbers() and getEvenNumbers() works", {
  
  expect_equal(getOddNumbers(1:10), c(1, 3, 5, 7, 9))
  expect_equal(getEvenNumbers(1:10), c(2, 4, 6, 8, 10))
})

test_that("extendLimits() works", {

  expect_identical(extendLimits(c(-1, 1), 1, 1), c(-3, 3))
  expect_identical(extendLimits(c(-1, 1), 1, 1, absolute = TRUE), c(-2, 2))
})

test_that("assignAll() works", {

  expect_error(assignAll(1))
  expect_error(assignAll("a"))  
  
  assignAll(list(a = 1, b = 2))
  
  expect_true(all(c("a", "b") %in% ls(envir = .GlobalEnv)))
})

test_that("assignPackageObjects() works", {

  assignPackageObjects("kwb.utils")
  
  expect_true("assignPackageObjects" %in% ls(envir = .GlobalEnv))
}) 

test_that("getGlobally() works", {
  
  assign("a", 1, envir = .GlobalEnv)
  
  expect_true("a" %in% ls(envir = .GlobalEnv))
  
  expect_identical(getGlobally("a"), 1)
  
  expect_null(getGlobally("no_such_object"))
})

test_that("breakInSequence() works", {

  expect_error(breakInSequence("a"))
  expect_identical(breakInSequence(c(1, 2, 4, 5)), 2L)
  expect_identical(breakInSequence(c(1, 3, 5, 8, 10, 12), expectedDiff = 2), 3L)
})

test_that("warnIfEmpty() works", {

  expect_warning(warnIfEmpty(NULL))  
  expect_warning(warnIfEmpty(data.frame()))  
  expect_warning(warnIfEmpty(list()))
  expect_warning(warnIfEmpty(c()))
  
  expect_silent(data.frame(a = 1))
  expect_silent(list(a = 1))
  expect_silent(1)
})

test_that("hsMatrixToListForm() works", {

  df <- data.frame(a = 1:3, b = 11:13, c = 21:23)
  
  y <- hsMatrixToListForm(
    df, keyFields = "a", colNamePar = "par", colNameVal = "val"
  )
  
  expect_is(y, "data.frame")
  
  expect_identical(nrow(y), 6L)
  
  expect_identical(names(y), c("a", "par", "val"))
  
  x <- reshape(y, direction = "wide", timevar = "par", idvar = "a")

  expect_equal(
    structure(df, names = NULL), 
    structure(x, names = NULL, reshapeWide = NULL)
  )
})

test_that("hsSafeName() works", {

  existing <- c("a", "b")
  
  expect_identical(hsSafeName("c", existing), "c")
  
  expect_identical(hsSafeName("a", existing), "a_1")
  expect_identical(hsSafeName("a", c(existing, "a_1")), "a_2")
})
