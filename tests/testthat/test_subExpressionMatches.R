test_that("subExpressionMatches() works", {
  
  y1 <- subExpressionMatches(
    regularExpression = "(\\d{4})-(\\d{2})-(\\d{2})", 
    text = c("1975-01-14", "2003-01", "2015-08-20"),
    match.names = c("year", "month", "day")
  )
  
  expected1 <- list(
    list(year = "1975", month = "01", day = "14"),
    NULL,
    list(year = "2015", month = "08", day = "20")
  )
  
  y2 <- subExpressionMatches(
    regularExpression = "^([^.]+)\\.([^.]+)@(.*)$",
    text = c("hauke.sonnenberg@lernshow.de",
             "angela.merkel@germany.de"),
    match.names = c("firstName", "lastName", "host")
  )
  
  expected2 <- list(
    list(firstName = "hauke", lastName = "sonnenberg", host = "lernshow.de"),
    list(firstName = "angela", lastName = "merkel", host = "germany.de")
  )
  
  pattern <- "^(.*)\\.([^.]+)$"
  match.names = c("basename", "extension")
  
  y3 <- subExpressionMatches(pattern, "file.txt", match.names)
  y4 <- subExpressionMatches(pattern, "file.txt")
  
  expected3 <- list(basename = "file", extension = "txt")
  
  y5 <- subExpressionMatches(
    regularExpression = "(Spieler|Player)\\s+(\\d+)", 
    text = c("Spieler 1", "Player 21", "Spieler 311"),
    select = c(playerID = 2)
  )
  
  expected5 <- list(
    list(playerID = "1"),
    list(playerID = "21"),
    list(playerID = "311")
  )
  
  expect_identical(y1, expected1)
  expect_identical(y2, expected2)
  expect_identical(y3, expected3)
  expect_identical(y4, structure(expected3, names = NULL))
  expect_identical(y5, expected5)
})
