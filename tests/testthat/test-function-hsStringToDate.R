test_that("hsStringToDate() works", {

  f <- hsStringToDate
  
  expect_s3_class(f("2018-05-22"), "Date")
  expect_s3_class(f("22.05.2018", "%d.%m.%Y"), "Date")
  
  expect_error(f("22.05.2018"))
  
  expect_true(is.na(f(NA, "Y%-%m-%d")))
  result <- f(c(NA, "14.01.1975"), "%d.%m.%Y")
  expect_true(is.na(result[1L]), result[[2L]] == as.Date("1975-01-14"))
  
  expect_error(f("2022-06-04", dateFormat = "%d.%m.%Y"))
})
