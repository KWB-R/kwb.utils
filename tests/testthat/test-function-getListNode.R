test_that("getListNode() works", {

  f <- getListNode

  L <- list(
    a1 = list(
      b1 = list(c = 1, d = 2, e = 3),
      b2 = list(c = list(c1 = 1, c2 = 2, c3 = 3))
    ),
    a2 = list(b3 = 22, b4 = 44)
  )

  expect_identical(getListNode(L, "a1/b2/c/c2"), 2)
  expect_identical(getListNode(L, "a1/b2/c"), list(c1 = 1, c2 = 2, c3 = 3))
  expect_identical(getListNode(L, "a2/b3"), 22)
  expect_identical(getListNode(L, ""), L)
})
