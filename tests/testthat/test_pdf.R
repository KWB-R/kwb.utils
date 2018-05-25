test_that(
  
  paste(
    "preparePdfIf(), preparePdf(), finishAndShowPdfIf() and",  
    "finishAndShowPdf() work"
  ), {
    
  dev_list <- dev.list()
  
  expect_identical(preparePdfIf(FALSE), "")
  
  expect_identical(dev.list(), dev_list)
  
  pdf_file_1 <- preparePdfIf(TRUE)
  pdf_file_2 <- preparePdfIf(TRUE, PDF = tempfile(fileext = ".pdf"))
  pdf_file_3 <- preparePdf(makeCurrent = FALSE)
  
  on.exit(finishAndShowPdfIf(TRUE, pdf_file_1))
  on.exit(finishAndShowPdf(pdf_file_2), add = TRUE)
  on.exit(finishAndShowPdf(pdf_file_3), add = TRUE)
  
  expect_true(file.exists(pdf_file_1))
  expect_true(file.exists(pdf_file_2))
  expect_true(file.exists(pdf_file_3))
  
  n <- length(dev.list())

  expect_true(dev.cur() != dev.list()[n])  
  expect_identical(names(dev.list())[n], "pdf")
  expect_identical(names(dev.list())[n], "pdf")
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
  
  finishAndShowPdfIf(FALSE)
})

# finishAndShowPdf

test_that("hsPrepPdf() gives a warning", {

  expect_warning(hsPrepPdf(tempfile()))
  
  dev.off()
})

# hsShowPdf --------------------------------------------------------------------
