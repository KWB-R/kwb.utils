test_that("parallelNonNA() works", {
  
  expect_identical(parallelNonNA(c(1, NA, 3), c(NA, 2, NA)), c("1", "2", "3"))

  expect_identical(parallelNonNA(c(1, NA, NA), c(NA, 2, NA)), c("1", "2", ""))

  expect_warning(y <- parallelNonNA(c(1, 2, 3), c(1, 2, 4)))

  expect_equal(attr(y, "invalid"), data.frame(index = 3, a = 3, b = 4))
})
