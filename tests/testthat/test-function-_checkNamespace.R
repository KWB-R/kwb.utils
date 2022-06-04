test_that(".checkNamespace() works", {

  expect_error(.checkNamespace("a", "b"))
  
  expect_null(.checkNamespace("kwb.utils", "selectColumns"))
})
