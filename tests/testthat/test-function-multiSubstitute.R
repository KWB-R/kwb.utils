test_that("multiSubstitute() works", {
  
  expect_identical(multiSubstitute("abc", list(a = "b", b = "c")), "ccc")
  expect_identical(multiSubstitute("abc", list(a = "b", "^b" = "c")), "cbc")
  
  expect_output(multiSubstitute("abc", list(a = "A"), dbg = TRUE))
})
