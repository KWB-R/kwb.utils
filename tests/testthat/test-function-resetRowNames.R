test_that("resetRowNames() works", {

  persons <- data.frame(id = c(1, 2, 3), name = c("Peter", "Paul", "Mary"))

  persons <- persons[order(persons$name), ]
  
  expect_identical(
    rownames(resetRowNames(persons)), 
    as.character(seq_len(nrow(persons)))
  )
  
  expect_error(resetRowNames(1))
})
