test_that("expandGrid() works", {
  
  persons <- c("Peter", "Paul", "Mary")
  
  fruits <- c("apple", "pear")

  grid <- expandGrid(person = persons, fruit = fruits)

  expect_true(is.data.frame(grid))
  expect_identical(names(grid), c("person", "fruit"))
  expect_identical(nrow(grid), length(persons) * length(fruits))
})
