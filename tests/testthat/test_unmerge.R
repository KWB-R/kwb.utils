# checkResult ------------------------------------------------------------------
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

# T E S T ----------------------------------------------------------------------
test_that("merging the unmerged results in the original data frame", {
  
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

test_that("almostEqual works", {
  expect_error(1, 1:2)
  expect_error(1:2, 3)
  expect_false(1, 2)
  expect_true(1, 1)
  expect_false(1, 1.1, 0.1)
  expect_true(1, 1.09, 0.1)
})

test_that("allAreEqual works", {
  expect_true(c(1, 1, 1))
  expect_false(c(1, 1, 2))
})
