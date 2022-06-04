test_that("hsStringToDate() works", {

  expect_s3_class(hsStringToDate("2018-05-22"), "Date")
  expect_s3_class(hsStringToDate("22.05.2018", "%d.%m.%Y"), "Date")
  
  expect_error(hsStringToDate("22.05.2018"))
  
  expect_true(is.na(hsStringToDate(NA, "Y%-%m-%d")))
  result <- hsStringToDate(c(NA, "14.01.1975"), "%d.%m.%Y")
  expect_true(is.na(result[1L]), result[[2L]] == as.Date("1975-01-14"))
})
