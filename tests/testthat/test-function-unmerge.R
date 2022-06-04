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
