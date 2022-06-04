test_that("findChanges() works", {
  
  f <- kwb.utils::findChanges
  
  expect_null(f(character()))
  expect_null(f(integer()))
  expect_null(f(numeric()))
  
  expect_identical(f(1:3),data.frame(
    starts_at = 1:3, 
    ends_at = 1:3, value = 1:3
  ))
  
  expect_identical(f(c("a", "b", "b", "c")), data.frame(
    starts_at = c(1L, 2L, 4L),
    ends_at = c(1L, 3L, 4L), 
    value = c("a", "b", "c"),
    stringsAsFactors = FALSE
  ))

  # Take care with real numbers!
  #expect_true(nrow(f(c(1, sqrt(2)^2 - 1))) == 1L)
})
