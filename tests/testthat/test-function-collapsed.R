test_that("collapsed() works", {
  
  expect_equal(collapsed(c("a", "b")), "a b")
  expect_equal(collapsed(1:5, "@"), "1@2@3@4@5")
})
