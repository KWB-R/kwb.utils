test_that("get_homedir() works", {
  
  home <- get_homedir()
  
  expect_true(grepl(user(), home))
  expect_true(file.exists(home))
})
