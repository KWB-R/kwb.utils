test_that("toLookupList() works", {
  
  keys <- c("A", "B", "C")
  values <- c("Apple", "Banana", "Cherry")
  
  y <- toLookupList(keys, values)
  
  expect_true(is.list(y))
  expect_length(y, length(keys))
  expect_identical(names(y), keys)
  expect_identical(as.character(y), values)
  
  expect_error(toLookupList(data = data.frame(Key = LETTERS[1:5])))
  
  data <- data.frame(Key = 1:26, Value = paste("Letter", LETTERS))
  
  y <- toLookupList(data = data)

  expect_identical(hsTrim(names(y)), as.character(data$Key))
  expect_identical(as.character(y), as.character(data$Value))
})
