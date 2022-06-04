test_that("shorten() works", {
  
  x <- "Fest gemauert in der Erden steht die Form aus Lehm gebrannt"
  
  y1 <- shorten(x)
  y2 <- shorten(x, delimiter = "@")
  y3 <- shorten(x, 20)

  expect_equal(nchar(y1), 10)
  expect_equal(nchar(y2), 10)
  expect_equal(nchar(y3), 20)
  
  expect_equal(y1, "Fest...nnt")
  expect_equal(y2, "Fest @annt")
  expect_equal(y3, "Fest gema...gebrannt")
})
