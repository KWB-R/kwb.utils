test_that("stopIfNotEnoughColumns() works", {

  f <- kwb.utils:::stopIfNotEnoughColumns

  expect_error(f())

  headerFields <- c("a", "b", "c")
  
  columnDescription_1 <- list(
    columnDescriptor("a"),
    columnDescriptor("b"),
    columnDescriptor("c"),
    columnDescriptor("d")
  )
  
  columnDescription_2 <- list(
    z = columnDescriptor("z")
  )
  
  expect_error(f(headerFields, columnDescription_1, ","))
  expect_silent(f(headerFields, columnDescription_2, ","))
  
})
