test_that("createStorage() works", {

  f <- createStorage
  
  expect_error(f())
  
  expect_output(storage <- f(tempdir()), "already exists")
  
  expect_is(storage, "list")
  expect_identical(storage$path, tempdir())

})
