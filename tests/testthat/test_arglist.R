test_that("arglist works as expected", {
  
  expect_identical(
    arglist(a = 1, b = 2, c = 3), 
    list(a = 1, b = 2,c = 3))
  
  expect_identical(
    arglist(list(a = 1, b = 2), list(b = 3), c = 4),
    list(a = 1, b = 3, c = 4))
  
  expect_identical(
    arglist(list(xlim = c(0, 20), ylim = c(0, 100)), xlim = c(20, 40)),
    list(xlim = c(20, 40), ylim = c(0, 100)))
  
  expect_identical(
    arglist(list(xlim = c(0, 20), ylim = c(0, 100)), zlim = c(-1, 1)),
    list(xlim = c(0, 20), ylim = c(0, 100), zlim = c(-1, 1)))
})

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
