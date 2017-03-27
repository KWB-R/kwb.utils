file <- system.file("extdata", "dictionary.txt", package = "kwb.utils")
dictionary <- readDictionary(file)

test_that("resolve works correctly", {
  
  expect_identical(length(resolve(dictionary)), length(dictionary))
  expect_match(resolve(dictionary, extension = "pdf")$file.out, "pdf$")
  expect_match(resolve("dir.project", dictionary), "example")
  expect_match(resolve("file.out", dictionary, extension = "pdf"), "pdf$")
  expect_match(resolve("dir.project", dictionary, project = "new"), "new")
  expect_identical(resolve("a"), "a")
  expect_identical(resolve(c("a", "b", "c")), c("a", "b", "c"))
  expect_identical(resolve("a", list(a = "b")), "b")
  expect_identical(resolve(c("<a>/<b>", "a/<c>"), 
                           list(a = "A"), b = "B", c = "C"), c("A/B", "a/C"))
  expect_identical(resolve(list(a = "<b>", b = "c")), list(a = "c", b = "c"))
  expect_identical(resolve(list(a = "<b>"), b = "bbb"), list(a = "bbb"))
})

test_that("hsResolve does not crash on unresolved placeholders", {
  expect_equal(hsResolve("<a>"), "<a>")
  expect_equal(hsResolve("<a>", list(b = "c")), "<a>")
})

test_that("hsResolve does not 'consume' backslashes", {
  expect_equal(hsResolve("<a>", list(), a = "\\"), "\\")
  expect_equal(hsResolve("<a>", list(a = "\\")), "\\")
})

test_that("hsResolve replaces recursively", {
  expect_equal(hsResolve("x", list(x = 1)), "1")
  expect_equal(hsResolve("x", list(x = "<y>", y = 1)), "1")
  expect_equal(hsResolve("x", list(x = "<y>", y = "<z>", z = "22")), "22")
})

test_that("hsResolve works with a vector of elements", {
  expect_equal(hsResolve(c("x", "y", "z"), list(x = 1, y = 2, z = 3)), 
               as.character(1:3))
  expect_equal(hsResolve(c("x", "y", "z"), list(x = 1, y = 2), z = 3),
               as.character(1:3))
})

test_that("resolveAll works as expected", {
  expect_equal(resolveAll(list(x = 1, y = "<x>", z = "<y>")), 
               list(x = "1", y = "1", z = "1"))
  expect_equal(resolveAll(list(x = 1, y = "<a>", z = "<y>"), a = 2), 
               list(x = "1", y = "2", z = "2"))
})

test_that("getTagNames works as expected", {
  expect_error(getTagNames(1), "must be .* character")
  expect_error(getTagNames(c("<a>", "<b>"), expected.length = 1, "length 1"))
  expect_identical(getTagNames(c("<a>", "<b>")), list("a", "b"))
})
