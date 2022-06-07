test_that("createStorage() works", {

  f <- createStorage
  
  expect_error(f())
  
  expect_output(storage <- f(tempdir()), "already exists")
  expect_is(storage, "list")
  expect_identical(storage$path, tempdir())
  expect_is(storage$list(), "character")
  
  #storage$save(abc = 1, def = 2, .overwrite = TRUE)
  #expect_true("abc.rds" %in% storage$list())
  #result <- storage$load("abc")
  #expect_identical(result, 1)
  #storage$remove("abc")
  #expect_null(expect_message(storage$load("abc")))
})
