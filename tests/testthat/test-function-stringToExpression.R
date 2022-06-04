test_that("stringToExpression() works", {

  expect_true(is.expression(stringToExpression("a + b")))
})
