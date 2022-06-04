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
