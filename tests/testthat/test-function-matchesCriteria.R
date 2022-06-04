test_that("matchesCriteria() works", {
  
  Data <- data.frame(A = c("x", "y", "z", NA), B = c(1, 2, NA, 4))
  
  criteria_1 <- c("A %in% c('y', 'z')", "B %% 2 == 0")
  
  expect_warning(y <- matchesCriteria(Data, criteria_1, dbg = FALSE))
  expect_identical(y, c(FALSE, TRUE, NA, FALSE))
  
  y <- matchesCriteria(Data, criteria_1[1], dbg = FALSE)
  expect_identical(y, c(FALSE, TRUE, TRUE, FALSE))
  
  expect_warning(y <- matchesCriteria(Data, criteria_1[2], dbg = FALSE))
  expect_identical(y, c(FALSE, TRUE, NA, TRUE))

  expect_error(matchesCriteria(Data, "AB %in% c('y', 'z')", dbg = FALSE))

  expect_true(all(matchesCriteria(Data, dbg = FALSE)))
  
  expect_message(y <- matchesCriteria(
    Data, criteria_1, add.details = TRUE, na.to.false = TRUE, dbg = FALSE
  ))

  expect_identical(removeAttributes(y), c(FALSE, TRUE, FALSE, FALSE))
  
  details <- attr(y, "details")
  
  expect_true(! is.null(criteria <- attr(details, "criteria")))
  
  expect_identical(as.character(criteria$condition), criteria_1)
  
  expect_error(matchesCriteria(Data, "sum(B, na.rm = TRUE)", dbg = FALSE))
})
