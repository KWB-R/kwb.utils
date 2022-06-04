test_that("countOrSum() works", {

  x <- data.frame(
    Group = rep(c("A", "B", "C"), 4),
    Even = rep(c(FALSE, TRUE), 6),
    Value = seq_len(12)
  )

  y_1 <- countOrSum(x, "Group")
  y_2 <- countOrSum(x, c("Group", "Even"))
  y_3 <- countOrSum(x, "Group", sum.up = "Value")
  y_4 <- countOrSum(x, c("Group", "Even"), sum.up = "Value")
  
  expect_true(all(y_1 == 4))
  expect_identical(dim(y_1), 3L)
  expect_identical(dim(y_2), c(3L, 2L))
  expect_identical(dim(y_3), 3L)
  expect_identical(dim(y_4), c(3L, 2L))
  
  n <- nrow(x)
  
  expect_identical(sum(y_1), n)
  expect_identical(sum(y_2), n)
  
  S <- sum(x$Value)

  expect_identical(sum(y_3), S)
  expect_identical(sum(y_4), S)
})
