test_that("copyListElements() works", {
  
  x <- list(list(a = 1), list(b = 2), list(c = 3))
  y <- list("b1", "b2", "b3")
  
  expect_error(copyListElements("a", 1))
  expect_error(copyListElements(list(), 1))
  expect_error(copyListElements(list(list(a = 1)), list(b = 2, c = 3)))
  expect_error(copyListElements(list(list()), list(a = 1), name = character()))
  
  z <- copyListElements(x, y)
  
  expect_is(z, "list")
  expect_length(z, length(x))
})

test_that("excludeNULL() works", {

  L <- list(a = 1, b = NULL, c = "three")

  expect_error(excludeNULL(0))
  expect_identical(sum(sapply(excludeNULL(L), is.null)), 0L)
})

test_that("toNamedList() works", {
  
  x_vector <- c("Peter", "Paul", "Mary")
  
  x_list <- toNamedList(x_vector)
  
  expect_true(all(names(x_list) == x_vector))
})

test_that("nameByElement() works", {
  
  L <- list(
    list(group = "A", value = 1),
    list(group = "B", value = 2)
  )

  y <- nameByElement(L, "group")

  expect_is(y, "list")
  expect_identical(names(y), c("A", "B"))
})

test_that("removeElements() works", {
  
  x <- list(a = 1, b = 2:3, c = 3:5)
  
  y <- removeElements(x, elements = "a")
  
  expect_is(y, "list")
  expect_length(y, 2)
  expect_identical(y, x[c("b", "c")])
})
