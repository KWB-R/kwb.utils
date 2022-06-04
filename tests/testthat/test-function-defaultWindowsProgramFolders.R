test_that("defaultWindowsProgramFolders() works", {

  y <- defaultWindowsProgramFolders()
  
  expect_is(y, "character")
  expect_named(y)
})
