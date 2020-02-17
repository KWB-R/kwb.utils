test_that("selectElements() works", {
  
  L <- list(a = 1, b = "one", c = data.frame(x = 1:2, y = 3:4))
  
  expect_error(selectElements())
  expect_error(selectElements(1))
  expect_error(selectElements(L))
  expect_error(selectElements(L, "no_such_element"))
  expect_warning(selectElements(L, "no_such_element", do.stop = FALSE))
  expect_silent(selectElements(
    L, "no_such_element", do.stop = FALSE, do.warn = FALSE
  ))
  
  expect_identical(selectElements(L, character()), list())
  
  expect_equal(selectElements(L, "a"), 1)
  expect_equal(kwb.utils::selectElements(L, c(b = "a")), 1)
  
  expect_equal(selectElements(L, "b"), "one")
  expect_identical(selectElements(L, c("a", "c")), L[c("a", "c")])

  y <- selectElements(L, c(A = "a", C = "c"))

  expect_identical(unname(y), unname(L[c("a", "c")]))
  expect_identical(names(y), c("A", "C"))
})
