test_that("commaCollapsed() works", {
  
  expect_equal(commaCollapsed(c("a", "b")), "a,b")
  expect_equal(commaCollapsed(1:5), "1,2,3,4,5")
})
