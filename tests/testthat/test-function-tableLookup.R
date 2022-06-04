test_that("tableLookup() works", {

  keys <- c("A", "B", "C")
  values <- c("Apple", "Banana", "Cherry")
  
  x <- toLookupTable(keys, values, as.twoColumnTable = TRUE)
  
  for (i in seq_along(keys)) {
    expect_identical(tableLookup(x, keys[i]), values[i])
  }
  
  default <- "default_value"
  
  expect_warning(y <- tableLookup(x, "no_such_key", default = default))
  expect_identical(y, default)
  
  x <- rbind(x, c("B", "Blueberry"))
  
  expect_warning(y <- tableLookup(x, "B"))
  expect_identical(y, x$value[which(x$key == "B")[1]])
})
