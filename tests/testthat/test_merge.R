test_that(".checkDimensions() works", {

  dim_1 <- list(x = c("x1", "x2", "x3"), y = c("y1", "y2", "y3"))
  
  .checkDimensions(dim_1, dim_1)
  
  dim_2 <- dim_1
  
  dim_2$y[1] <- "y11"
  
  expect_error(.checkDimensions(dim_1, dim_2))
  
  dim_2 <- list(x = 1:4, y = 1)
  
  expect_error(capture.output(.checkDimensions(dim_1, dim_2)))
})
