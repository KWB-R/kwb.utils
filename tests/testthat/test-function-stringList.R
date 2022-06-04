test_that("stringList() works", {
  
  expect_equal(stringList(c("a", "b")), "'a', 'b'")
  expect_equal(stringList(c("a", "b"), collapse = ","), "'a','b'")
  expect_equal(stringList(c("a", "b", "c"), "", "@"), "a@b@c")
})
