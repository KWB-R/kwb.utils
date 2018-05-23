checkResult <- function(x, xy)
{
  standardise <- function(x) kwb.utils::fullySorted(x[, sort(names(x))])
  
  x <- standardise(x)
  y <- standardise(merge(xy$x, xy$y))
  
  isIdentical <- identical(x, y)
  
  if (! isIdentical) {
    
    cat("str(x):\n"); str(x)
    cat("str(y):\n"); str(y)
  }
  
  isIdentical
}

test_that("unmerge() works", {
  
  z <- data.frame(
    insp = c(1, 1, 2, 3, 4, 5),
    pipe = c(1, 1, 1, 2, 2, 3),
    p1 = c("a", "a", "a", "b", "b", "c"),
    p2 = c("A", "A", "A", "B", "B", "c"),
    i1 = c(100, 150, 200, 100, 200, 100)
  )
  
  expect_true(checkResult(z, unmerge(z, "insp")))
  expect_true(checkResult(z, unmerge(z, "i1")))
  expect_true(checkResult(z, unmerge(z, "pipe")))
  expect_true(checkResult(z, unmerge(z, "p1")))
  expect_true(checkResult(z, unmerge(z, "p2")))
  expect_true(checkResult(z, unmerge(z, c("p1", "p2"))))
})

test_that("mergeAll() and safeMerge() work", {
  
  to_data <- function(...) data.frame(stringsAsFactors = FALSE, ...)
  
  peter <- to_data(fruit = c("apple", "pear", "banana"), kg = 1:3)
  paul <- to_data(fruit = c("banana", "apple", "lemon"), kg = c(10, 20, 30))
  mary <- to_data(fruit = c("lemon", "organger", "apple"), kg = c(22, 33, 44))

  expect_error(safeMerge(peter, paul, by = "no_such_column"))
  expect_silent(y <- safeMerge(peter, paul, by = "fruit"))
  
  expect_identical(y, merge(peter, paul, by = "fruit"))
  
  x <- list(peter = peter, paul = paul, mary = mary)
  
  expect_output(y_1 <- mergeAll(x, by = "fruit"))
  expect_output(y_2 <- mergeAll(x, by = "fruit", all = TRUE))
  
  expect_silent(mergeAll(x, by = "fruit", dbg = FALSE))

  expect_identical(nrow(y_1), 1L)
  
  expect_identical(
    sort(y_2$fruit), 
    sort(unique(c(peter$fruit, paul$fruit, mary$fruit)))
  )
})

test_that("mergeNamedArrays works", {
  a1 <- array(
    1:12, dim = c(2, 4, 2), dimnames = list(
      paste0("x", 1:2), paste0("y", 1:4), paste0("z", 1:2)
  ))

  a2 <- array(
    11:16, dim = c(1, 3, 2), dimnames = list(
      "x3", paste0("y", 2:4), paste0("z", 1:2)
  ))

  a <- mergeNamedArrays(list(a1, a2))
  
  dim_names_1 <- dimnames(a1)
  dim_names_2 <- dimnames(a2)
  dim_names <- dimnames(a)

  for (i in 1:3) {
    expect_true(all(dim_names_1[[i]] %in% dim_names[[i]]))
    expect_true(all(dim_names_2[[i]] %in% dim_names[[i]]))
  }
})

test_that("dropDim works", {
  a <- array(1:8, dim = c(2, 2, 2), dimnames = list(
    paste0("x", 1:2), paste0("y", 1:2), paste0("z", 1:2)
  ))
  
  a1 <- dropDim(a[, 1, 1, drop = FALSE], dimension = 3)

  expect_identical(dim(a1), c(2L, 1L))
})
