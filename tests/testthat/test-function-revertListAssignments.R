test_that("revertListAssignments() works", {
  
  abbreviation <- list(de = "Germany", en = "England")

  y <- revertListAssignments(abbreviation)

  expect_true(is.list(y))
  
  expect_identical(names(y), as.character(abbreviation))
  expect_identical(as.character(y), names(abbreviation))
  
  expect_identical(
    abbreviation,
    revertListAssignments(revertListAssignments(abbreviation))
  )
})
