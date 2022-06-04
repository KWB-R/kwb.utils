test_that("nameByElement() works", {
  
  L <- list(
    list(group = "A", value = 1),
    list(group = "B", value = 2)
  )

  y <- nameByElement(L, "group")

  expect_is(y, "list")
  expect_identical(names(y), c("A", "B"))
})
