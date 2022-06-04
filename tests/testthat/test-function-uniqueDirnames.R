test_that("unique_dirnames() works", {

  expect_error(uniqueDirnames())
  expect_identical(uniqueDirnames("a/b"), "a")
  expect_identical(uniqueDirnames(c("a/b", "a/c")), "a")
  expect_identical(uniqueDirnames(c("a/b", "a/c", "b/d")), c("a", "b"))
  
})
