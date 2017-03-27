# test_roundColumns ------------------------------------------------------------
test_roundColumns <- function
### test_roundColumns
() 
{
  x <- data.frame(a = c(1.23456, 234.5678), b = c("A", "B"), c = c(1245, 234))

  x1 <- roundColumns(x, columnNames = c("a", "c"), digits = 2)
  x2 <- roundColumns(x, columnNames = c("a", "c"), digits = 1)
  x3 <- roundColumns(x, columnNames = c("a", "c"), digits = 0)   
  x4 <- roundColumns(x, digits = list(a = 1, c = -2))
}
