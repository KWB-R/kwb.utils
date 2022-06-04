test_that("renameColumns() and renameAndSelect() work", {
  
  Data <- data.frame(A = 1:5, B = 2:6)
  
  renamings = list(A = "Alpha", B = "Bravo")
  
  y <- renameColumns(Data, renamings)

  expect_warning(hsRenameColumns(y, renamings))
  
  expect_identical(
    suppressWarnings(hsRenameColumns(y, renamings)), 
    renameColumns(y, renamings)
  )
  
  expect_identical(names(y), as.character(renamings))
  
  expect_identical(unname(Data), unname(y))
  
  expect_identical(Data, renameColumns(Data, NULL))
  
  expect_identical(renameAndSelect(Data, renamings), y)
})
