#
# This test file has been generated by kwb.test::create_test_files()
# launched by user hsonne on 2022-06-04 18:39:17.
# Your are strongly encouraged to modify the dummy functions
# so that real cases are tested. You should then delete this comment.
#

test_that("checkNamespace() works", {

  f <- kwb.utils:::checkNamespace

  expect_error(
    f()
    # argument "packageName" is missing, with no default
  )

})
