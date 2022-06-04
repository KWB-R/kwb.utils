test_that("rbindAll() works", {

  L <- list(1:3, 4:6)
  
  y <- rbindAll(L, nameColumn = "Name")
  
  expect_is(y, "data.frame")
  expect_true("Name" %in% names(y))
  expect_identical(dim(y), c(2L, 4L))
  
  L <- list(
    A = data.frame(x = 1:2, y = 2:3),
    B = data.frame(x = 1:3, y = 2:4)
  )
  
  L_unnamed <- unname(L)

  y1 <- rbindAll(L)
  y2 <- rbindAll(L, nameColumn = "group")
  y3 <- rbindAll(L_unnamed, nameColumn = "group", namesAsFactor = FALSE)
  y4 <- rbindAll(L_unnamed, nameColumn = "group")
  
  expected1 <- data.frame(
    x = c(L$A$x, L$B$x),
    y = c(L$A$y, L$B$y)
  )
  
  expected2 <- cbind(
    expected1,
    group = as.factor(c(rep("A", nrow(L$A)), rep("B", nrow(L$B)))),
    stringsAsFactors = FALSE
  )
  
  expected3 <- cbind(
    expected1,
    group = c(rep(1L, nrow(L$A)), rep(2L, nrow(L$B)))
  )
  
  expected4 <- expected3
  expected4$group <- as.factor(expected4$group)
  
  expect_identical(y1, expected1)
  expect_identical(y2, expected2)
  expect_identical(y3, expected3)
  expect_identical(y4, expected4)
})
