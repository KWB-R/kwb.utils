test_that("headtail() works", {
  
  x <- data.frame(number = 1:26, letter = LETTERS)
  
  expect_error(headtail(1))
  expect_error(headtail(list(a = 1, b = 2)))
               
  out_1 <- capture.output(headtail(x))
  out_2 <- capture.output(headtail(x, 10))
  out_3 <- capture.output(headtail(x, 16))
  out_4 <- capture.output(headtail(x[10:19, ], 10))
  
  expect_length(out_1,  6 + 3)
  expect_length(out_2, 10 + 3)
  expect_length(out_3, 16 + 3)
  expect_length(out_4, 10 + 1)

  for (out in list(out_1, out_2, out_3)) {
  
    expect_length(grep("rows omitted", out), 1)
  }
})

test_that("readPackageFile() works", {

  expect_error(readPackageFile("no_such_file", "kwb.utils"))
  
  content <- readPackageFile(
    "dictionary.txt", "kwb.utils", sep = "=", header = FALSE, comment.char = "#"
  )
  
  expect_true(is.data.frame(content))
})

test_that("listObjects() and loadObject() work", {
  
  file <- tempfile()
  
  a <- 1
  b <- data.frame(x = 1, y = 2)
  
  save(a, b, file = file)

  y <- listObjects(file)
  expect_warning(getNamesOfObjectsInRDataFiles(file))
  
  expect_true(is.list(y))
  expect_identical(y[[1]], c("a", "b"))
  expect_identical(attr(y, "files"), file)
  
  expect_error(loadObject(file))
  expect_error(loadObject(file, "no_such_object"))
  
  expect_warning(getObjectFromRDataFile(file, "a"))
  expect_identical(loadObject(file, "a"), a)
  expect_identical(loadObject(file, "b"), b)
})

test_that("catLines() works", {

  text <- c("line 1", "line 2", "line 3")

  out <- capture.output(catLines(text))
  
  expect_identical(out, text)
})

test_that("the log functions work", {

  expect_identical(capture.output(.logstart(FALSE, "x")), character())
  expect_identical(capture.output(.logok(FALSE)), character())
  
  text <- "Hello, world!"
  
  expect_true(grepl(text, capture.output(.logstart(TRUE, text))))
  expect_true(grepl("ok", capture.output(.logok(TRUE))))
  
  expect_length(capture.output({
    .log(text)
    .log("<EOM>")
  }), 1)
  
  expect_length(capture.output({
    .logline(text)
    .logline("<EOM>")
  }), 2)
})

test_that("catIf() works", {

  text <- "Hello, world!"
  
  out_1 <- capture.output(catIf(FALSE, text))
  out_2 <- capture.output(catIf(TRUE, text))
  
  expect_identical(out_1, character())
  expect_identical(out_2, text)
})

test_that("printIf() works", {
  
  df <- data.frame(a = 1:2, b = c("A", "B"))
  
  out_1 <- capture.output(printIf(FALSE, 1))
  out_2 <- capture.output(printIf(TRUE, 1))
  out_3 <- capture.output(printIf(TRUE, df))
  
  expect_identical(out_1, character())
  expect_true(all(out_2 != ""))
  expect_length(out_2, 2)
  
  expect_identical(out_3[1], "df:")
})

test_that("clearConsole() works", {

  out <- capture.output(clearConsole())
  
  expect_true(out == "\014")
})

test_that("containsNulString() works", {
  
  file <- tempfile()
  
  writeBin(as.raw(c(0xff, 0xfe)), con = file)
  
  expect_true(containsNulString(file))
  
  writeBin(as.raw(c(0x55, 0xff, 0xfe)), con = file)
  
  expect_false(containsNulString(file))
})

test_that("writeText() works", {
  
  file <- tempfile()

  x <- c("Hello", "world")
  
  expect_output(writeText(x, file))

  type <- "welcome file"
  
  expect_output(writeText(x, file, type = type), regexp = type)

  expect_identical(x, readLines(file))
})
