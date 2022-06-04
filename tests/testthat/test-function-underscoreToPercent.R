test_that("underscoreToPercent() works", {
  
  expect_identical(underscoreToPercent("_"), "%")
  expect_identical(underscoreToPercent("_Y_m_d"), "%Y%m%d")
})
