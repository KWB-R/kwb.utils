test_that("indent() works", {
  
  expect_error(indent())
  expect_identical(indent("a", 1L), "  a")
  expect_identical(indent("a", 2L), "    a")
  expect_identical(indent("a", 1L, 4L), "    a")
})
