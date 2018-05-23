test_that("expandGrid() works", {
  
  persons <- c("Peter", "Paul", "Mary")
  
  fruits <- c("apple", "pear")

  grid <- expandGrid(person = persons, fruit = fruits)

  expect_true(is.data.frame(grid))
  expect_identical(names(grid), c("person", "fruit"))
  expect_identical(nrow(grid), length(persons) * length(fruits))
})

test_that("fullySorted() works", {
  
  y_1 <- fullySorted(head(iris))
  y_2 <- fullySorted(head(iris), renumber.rows = FALSE)
  y_3 <- fullySorted(head(iris), decreasing = TRUE)
  
  expect_false(is.unsorted(as.integer(rownames(y_1))))
  expect_true(is.unsorted(as.integer(rownames(y_2))))
  
  expect_identical(y_1, resetRowNames(y_3[seq(nrow(y_3), 1), ]))
})

test_that("splitIntoFixSizedBlocks() works", {

  y <- splitIntoFixSizedBlocks(iris, 50)
  
  expect_true(is.list(y))
  
  expect_true(all(sapply(y, is.data.frame)))
  
  expect_identical(sum(sapply(y, nrow)), nrow(iris))
  
  for (column in names(iris)) {
  
    expect_true(all(unlist(lapply(y, "[[", column)) == iris[[column]]))
  }
})

test_that("resetRowNames() works", {

  persons <- data.frame(id = c(1, 2, 3), name = c("Peter", "Paul", "Mary"))

  persons <- persons[order(persons$name), ]
  
  expect_identical(
    rownames(resetRowNames(persons)), 
    as.character(seq_len(nrow(persons)))
  )
  
  expect_error(resetRowNames(1))
})

test_that("frequencyTable() works", {
  
  data <- data.frame(
    A = c("a1", "a2", "a1", "a1", "a2", "", "a2", NA, "a1"),
    B = c("b1", "b1", NA, "b2", "b2", "b1", " ", "b3", "b2")
  )

  y_df <- frequencyTable(data)
  y_list <- frequencyTable(data, as.data.frame = FALSE)
  
  expect_true(is.data.frame(y_df))
  expect_true(is.list(y_list))
  
  elements <- c("column", "value", "count")
  
  expect_identical(names(y_df), elements)
  expect_identical(names(y_list), names(data))
  
  for (y in y_list) {
    
    expect_identical(names(y), elements)
  }
})

test_that("compareDataFrames() works", {

  x <- data.frame(a = 1:2, b = 2:3)
  y <- x
  
  expect_true(all(unlist(compareDataFrames(x, y))))
  
  z <- compareDataFrames(x, y[, c("b", "a")])
  
  expect_identical(
    names(which(! unlist(z))),
    c("identical", "identicalExceptAttributes", "sameColumnNames")
  )
  
  expect_output(tmp <- compareDataFrames(x, y, dbg = TRUE))
})

test_that("compareSets() works", {
  
  out_1 <- capture.output(compareSets(1:10, 3:13))
  out_2 <- capture.output(compareSets(1:10, 3:13, "numbers", "set 1", "set 2"))
  
  expect_true(grepl(": 1, 2", out_1[1]))
  expect_true(grepl(": 11, 12, 13", out_1[2]))
  
  expect_true(all(grepl("numbers", out_2)))
  expect_true(all(grepl("set 1", out_2)))
  expect_true(all(grepl("set 2", out_2)))
})

test_that("atLeastOneRowIn() works", {
  
  expect_true(atLeastOneRowIn(data.frame(a = 1)))
  expect_true(atLeastOneRowIn(matrix(1)))
  
  expect_false(atLeastOneRowIn(data.frame(a = character())))
})

test_that("rbindAll() works", {
  
  L <- list(
    A = data.frame(x = 1:2, y = 2:3),
    B = data.frame(x = 1:3, y = 2:4)
  )
  
  L_unnamed <- unname(L)

  y1 <- rbindAll(L)
  y2 <- rbindAll(L, nameColumn = "group")
  y3 <- rbindAll(L_unnamed, nameColumn = "group", namesAsFactor = FALSE)
  y4 <- rbindAll(L_unnamed, nameColumn = "group")
  
  expected1 <- data.frame(
    x = c(L$A$x, L$B$x),
    y = c(L$A$y, L$B$y)
  )
  
  expected2 <- cbind(
    expected1,
    group = as.factor(c(rep("A", nrow(L$A)), rep("B", nrow(L$B)))),
    stringsAsFactors = FALSE
  )
  
  expected3 <- cbind(
    expected1,
    group = c(rep(1L, nrow(L$A)), rep(2L, nrow(L$B)))
  )
  
  expected4 <- expected3
  expected4$group <- as.factor(expected4$group)
  
  expect_identical(y1, expected1)
  expect_identical(y2, expected2)
  expect_identical(y3, expected3)
  expect_identical(y4, expected4)
})

test_that("safeRowBindOfListElements() works", {

  x <- list(
    list(
      number = 1,
      data = data.frame(x = 1:2, y = 2:3)
    ),
    list(
      number = 2,
      data = data.frame(x = 11:12, y = 12:13)
    )
  )

  y_1 <- safeRowBindOfListElements(x, "data")

  x[[1]]$data$z <- 13:14
  
  y_2 <- safeRowBindOfListElements(x, "data")
  
  expect_true(is.data.frame(y_1))
  expect_true(is.data.frame(y_2))
  
  expect_identical(dim(y_1), c(4L, 2L))
  expect_identical(dim(y_2), c(4L, 3L))
})

test_that("safeRowBind() and safeRowBindAll() work", {
  
  data_1 <- data.frame(A = 1:2, B = 2:3)
  data_2 <- data.frame(B = 3:4, C = 4:5)
  
  y <- safeRowBind(data_1, data_2)
  
  expect_true(is.data.frame(y))
  expect_identical(dim(y), c(4L, 3L))
  expect_identical(names(y), c("A", "B", "C"))
  
  expect_error(safeRowBind(1, 1))
  expect_error(safeRowBind(data.frame(a = 1), 1))
  
  expect_identical(safeRowBind(data_1, NULL), data_1)
  expect_identical(safeRowBind(NULL, data_2), data_2)
  
  expect_identical(safeRowBindAll(list(data_1, data_2)), y)
})

test_that("addRowWithName() works", {

  x <- data.frame(a = 1:2, b = 3:4)
  
  expect_error(addRowWithName(x, data.frame(a = 5:6, b = 7:8)))
  expect_error(addRowWithName(x, data.frame(a = 10)))
               
  y <- addRowWithName(x, data.frame(a = 1 + 2, b = 3 + 4), "sum")
  
  expect_identical("sum", lastElement(rownames(y)))
})

test_that("moveToFront() works", {
  
  y <- moveToFront(1:10, 5)
  
  expect_true(is.numeric(y))
  expect_length(y, 10)
  expect_identical(y[1], 5)
  
  x <- c("a", "b", "c", "x", "y", "d")
  y <- moveToFront(x, c("x", "y"))
  
  expect_true(is.character(y))
  expect_length(y, length(x))
  expect_identical(y[1:2], c("x", "y"))
})
