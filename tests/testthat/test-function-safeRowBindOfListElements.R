test_that("safeRowBindOfListElements() works", {

  x <- list(
    list(
      number = 1,
      data = data.frame(x = 1:2, y = 2:3)
    ),
    list(
      number = 2,
      data = data.frame(x = 11:12, y = 12:13)
    )
  )

  y_1 <- safeRowBindOfListElements(x, "data")

  x[[1]]$data$z <- 13:14
  
  y_2 <- safeRowBindOfListElements(x, "data")
  
  expect_true(is.data.frame(y_1))
  expect_true(is.data.frame(y_2))
  
  expect_identical(dim(y_1), c(4L, 2L))
  expect_identical(dim(y_2), c(4L, 3L))
})
