test_that("mergeLists() works", {
  
  expect_identical(mergeLists(), list())
  
  expect_error(mergeLists(1))
  
  expect_error(mergeLists(list(a = 1), "b"))
  
  expect_warning(mergeLists(list(a = 1), NULL))
  
  expect_identical(
    mergeLists(list(a = 1), list(b = 2), list(c = 3)),
    list(a = 1, b = 2, c = 3)
  )
})
