test_that("hsQuoteChr() works", {

  expect_identical(hsQuoteChr("a"), "'a'")
  expect_identical(hsQuoteChr("He says: \"Hello\""), "'He says: \"Hello\"'")
  expect_identical(
    hsQuoteChr("He says: \"Hello\"", '"'), "\"He says: \"\"Hello\"\"\""
  )
  
  expect_null(hsQuoteChr(list()))
})
