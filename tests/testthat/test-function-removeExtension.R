test_that("removeExtension() works", {
  
  expect_equal(removeExtension("example.R"), "example")
  expect_equal(removeExtension("any/path/example.txt"), "any/path/example")
  expect_equal(removeExtension("a.b.c"), "a.b")
})
