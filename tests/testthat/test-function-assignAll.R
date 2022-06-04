test_that("assignAll() works", {

  expect_error(assignAll(1))
  expect_error(assignAll("a"))  
  
  assignAll(list(a = 1, b = 2))
  
  expect_true(all(c("a", "b") %in% ls(envir = .GlobalEnv)))
})
