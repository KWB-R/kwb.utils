context("string functions")

test_that("removeExtension() works", {
  
  expect_equal(removeExtension("example.R"), "example")
  expect_equal(removeExtension("any/path/example.txt"), "any/path/example")
  expect_equal(removeExtension("a.b.c"), "a.b")
})

test_that("shorten() works", {
  
  x <- "Fest gemauert in der Erden steht die Form aus Lehm gebrannt"
  
  y1 <- shorten(x)
  y2 <- shorten(x, delimiter = "@")
  y3 <- shorten(x, 20)

  expect_equal(nchar(y1), 10)
  expect_equal(nchar(y2), 10)
  expect_equal(nchar(y3), 20)
  
  expect_equal(y1, "Fest...nnt")
  expect_equal(y2, "Fest @annt")
  expect_equal(y3, "Fest gema...gebrannt")
})

test_that("fileExtension() works", {
  
  paths <- c("C:/example/file.csv", "file2.txt", "D:/e/f/ghi.jkl.zip")
  
  expect_equal(fileExtension(paths), c("csv", "txt", "zip"))
  expect_equal(fileExtension("C:/NEWS"), "")
})

test_that("pairwise() works", {
  
  x <- c("a.1", "b_hi", "c", "a.2", "d", "b_bye")
  
  expected <- c("a.1", "a.2", "b_hi", "b_bye", "c", "d")
  
  expect_equal(pairwise(x, starts = c("a.", "b_")), expected)
  expect_equal(pairwise(x, split = "[._]"), expected)
  
  expected <- c("a.1", "b_hi", "b_bye", "c", "a.2", "d")
  expect_equal(pairwise(x, split = "_"), expected)
  expect_equal(pairwise(x), expected)
})

test_that("appendSuffix() works", {
  
  values <- c("a", "b", "c")
  
  expect_equal(appendSuffix(values, ".1"), c("a.1", "b.1", "c.1"))
  
  expect_equal(
    appendSuffix(values, ".1", valuesToOmit = "c"), c("a.1", "b.1", "c")
  )
})

test_that("hsCountInStr() works", {
  
  x <- "Nananananananana"
  
  expect_identical(hsCountInStr("a", x), 8L)
  expect_identical(hsCountInStr("na", x), 7L)
  expect_identical(hsCountInStr("nanana", x), 2L)
  expect_identical(hsCountInStr("(na){3}", x), 2L)
})

test_that("csvTextToDataFrame() works", {
  
  y <- csvTextToDataFrame("a,b,c\n1,2,3", header = TRUE, sep = ",")
  
  expect_equal(dim(y), c(1, 3))
  expect_named(y)
  expect_equal(names(y), c("a", "b", "c"))
})

test_that("stringList() works", {
  
  expect_equal(stringList(c("a", "b")), "'a', 'b'")
  expect_equal(stringList(c("a", "b"), collapse = ","), "'a','b'")
  expect_equal(stringList(c("a", "b", "c"), "", "@"), "a@b@c")
})

test_that("commaCollapsed() works", {
  
  expect_equal(commaCollapsed(c("a", "b")), "a,b")
  expect_equal(commaCollapsed(1:5), "1,2,3,4,5")
})

test_that("collapsed() works", {
  
  expect_equal(collapsed(c("a", "b")), "a b")
  expect_equal(collapsed(1:5, "@"), "1@2@3@4@5")
})
