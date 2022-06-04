test_that("createIdAlong() works", {

  values <- 1:9
  expect_identical(createIdAlong(values, "base"), sprintf("base_%02d", values))
  expect_identical(createIdAlong(values), sprintf("value_%02d", values))
  expect_true(grepl("_ff$", rev(createIdAlong(1:255))[1]))
})
