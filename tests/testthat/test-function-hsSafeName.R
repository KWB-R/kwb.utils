test_that("hsSafeName() works", {

  existing <- c("a", "b")
  
  expect_identical(hsSafeName("c", existing), "c")
  
  expect_identical(hsSafeName("a", existing), "a_1")
  expect_identical(hsSafeName("a", c(existing, "a_1")), "a_2")
})
