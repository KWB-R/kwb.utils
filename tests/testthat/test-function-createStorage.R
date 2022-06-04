test_that("createStorage() works", {

  f <- createStorage
  
  expect_error(f())
  
  storage <- f(tempdir())
  
  expect_is(storage, "list")
  expect_identical(storage$path, tempdir())

})
