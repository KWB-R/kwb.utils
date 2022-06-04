skip_if(.OStype() != "windows")

test_that("hsOpenWindowsExplorer() works", {

  expect_silent(hsOpenWindowsExplorer())

})
