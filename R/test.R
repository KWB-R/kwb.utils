# test_roundColumns ------------------------------------------------------------

#' test roundColumns
#' 
test_roundColumns <- function() 
{
  x <- data.frame(a = c(1.23456, 234.5678), b = c("A", "B"), c = c(1245, 234))

  x1 <- roundColumns(x, columnNames = c("a", "c"), digits = 2)
  
  x2 <- roundColumns(x, columnNames = c("a", "c"), digits = 1)
  
  x3 <- roundColumns(x, columnNames = c("a", "c"), digits = 0)   
  
  x4 <- roundColumns(x, digits = list(a = 1, c = -2))
}

# test_matchesCriteria ---------------------------------------------------------

#' test matchesCriteria
#' 
test_matchesCriteria <- function()
{
  Data <- data.frame(A = c("x", "y", "z"),
                     B = c( 1,   2,   3))
  
  #Data[matchesCriteria(Data, list(inSet = list(A = c("y", "z"), B = 1:3))), ]
  criteria1 <- c("A %in% c('y', 'z')", "B %in% 1:3")
  
  criteria2 <- c("AB %in% c('y', 'z')", "B <- 1")
  
  Data[matchesCriteria(Data, criteria1), ]
  
  Data[matchesCriteria(Data, criteria2), ]
  
  Data[matchesCriteria(Data), ]
}
