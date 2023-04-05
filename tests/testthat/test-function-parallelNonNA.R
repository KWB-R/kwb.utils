test_that("parallelNonNA() works", {
  
  expect_identical(parallelNonNA(c(1, NA, 3), c(NA, 2, NA)), c(1, 2, 3))

  expect_identical(parallelNonNA(c(1L, NA, NA), c(NA, 2L, NA)), c(1L, 2L, NA))

  expect_warning(y <- parallelNonNA(c(1, 2, 33), c(1, 2, 44)))

  expect_equal(attr(y, "invalid"), data.frame(index = 3, a = 33, b = 44))
})
