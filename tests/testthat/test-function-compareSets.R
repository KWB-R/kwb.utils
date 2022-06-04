test_that("compareSets() works", {
  
  out_1 <- capture.output(compareSets(1:10, 3:13))
  out_2 <- capture.output(compareSets(1:10, 3:13, "numbers", "set 1", "set 2"))
  
  expect_true(grepl(": 1, 2", out_1[1]))
  expect_true(grepl(": 11, 12, 13", out_1[2]))
  
  expect_true(all(grepl("numbers", out_2)))
  expect_true(all(grepl("set 1", out_2)))
  expect_true(all(grepl("set 2", out_2)))
})
