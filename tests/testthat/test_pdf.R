test_that("preparePdfIf() works", {

  dev_list <- dev.list()
  
  expect_identical(preparePdfIf(FALSE), "")
  
  expect_identical(dev.list(), dev_list)
  
  pdf_file <- preparePdfIf(TRUE)
  
  on.exit(dev.off())
  
  expect_true(file.exists(pdf_file))
  
  expect_identical(names(lastElement(dev.list())), "pdf")
})

# preparePdf

test_that(".defaultWidth() and .defaultHeight() work", {

  expect_identical(.defaultWidth(landscape = TRUE, 0, 0), DIN.A4()$height.cm)
  expect_identical(.defaultWidth(landscape = FALSE, 0, 0), DIN.A4()$width.cm)
  
  expect_identical(.defaultHeight(landscape = TRUE, 0, 0), DIN.A4()$width.cm)
  expect_identical(.defaultHeight(landscape = FALSE, 0, 0), DIN.A4()$height.cm)
})

test_that("DIN.A4() works", {
  
  y <- DIN.A4()
  
  expect_is(y, "list")
  
  expect_identical(names(y), c("height.cm", "width.cm"))
})

# finishAndShowPdfIf

test_that("finishAndShowPdfIf() works", {
  
  finishAndShowPdf(FALSE)
})

# finishAndShowPdf

test_that("hsPrepPdf() gives a warning", {

  expect_warning(hsPrepPdf(FALSE))
})

# hsShowPdf --------------------------------------------------------------------
