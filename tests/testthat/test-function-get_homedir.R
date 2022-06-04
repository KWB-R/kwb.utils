test_that("get_homedir() works", {
  
  expect_is(get_homedir("unix"), "character")
  expect_is(get_homedir("windows"), "character")

  home <- get_homedir()
  
  expect_true(grepl(user(), home))
  expect_true(file.exists(home))
})
