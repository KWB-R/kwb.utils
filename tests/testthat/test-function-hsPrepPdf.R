test_that("hsPrepPdf() gives a warning", {

  expect_warning(hsPrepPdf(tempfile()))
  
  dev.off()
})
