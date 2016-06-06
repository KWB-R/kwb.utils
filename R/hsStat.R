# hsMovingMean -----------------------------------------------------------------
hsMovingMean <- structure(
  function # moving mean
### Calculate moving mean of \emph{n} values "around" values
(
  x,
  ### vector of values of which moving mean is to be calculated
  n,
  ### number of values "around" the values in \emph{x}, including the values in 
  ### \emph{x}, of which the mean is calculated. Only odd numbers 1, 3, 5, ...
  ### allowed. For each x[i] in x the moving mean is calculated by: 
  ### (x[i-(n-1)/2] + ... + x[i-1] + x[i] + x[i+1] + ... + x[i+(n-1)/2]) / n
  na.rm = FALSE
  ### logical. Should missing values (including NaN) be omitted from the
  ### calculations?
)
{
  if (!isOddNumber(n)) {
    stop(paste("The number n of values to be taken into account for the mean",
               "needs to be an odd number."))
  }
  matrix.values <- c(rep(c(x, rep(NA, n)), n-1), x)
  m <- matrix(matrix.values, ncol = n)
  means <- rowSums(m, na.rm=na.rm)/rowSums(!is.na(m))
  
  n.remove <- (n-1)/2  
  if (n.remove > 0) {
    i.at.beg <- seq(1, by=1, length.out=n.remove)
    i.at.end <- seq(nrow(m), by=-1, length.out=n.remove)
    means <- means[-c(i.at.beg, i.at.end)]
  }
  
  means
  ### Vector of moving means with the same number of values as there are in 
  ### \emph{x}. If na.rm is FALSE, the first \emph{(n-1)/2} values and the last
  ### \emph{(n-1)/2} values are NA since there are not enough values at the
  ### start and the end of the vector to calculate the mean.
}, ex = function() {
  x <- rnorm(30)
  
  plot(x, type = "b", main = "Moving mean over 3, 5, 7 points")
  
  times <- 2:4
  
  for (i in times) {
    lines(hsMovingMean(x, n = 2*i - 1), col = i, type = "b", lwd =  2)
  }
  
  legend("topright", fill = times, legend = sprintf("n = %d", 2*times - 1)) 
})

# percentageOfMaximum ----------------------------------------------------------
percentageOfMaximum <- function # percentageOfMaximum
### percentageOfMaximum
(
  x,
  ### vector of numeric values
  na.rm = TRUE
  ### passed to \code{max}
)
{
  100 * x / max(x, na.rm = na.rm)
  ### 100 * x / max(x)
}

# percentage -------------------------------------------------------------------
percentage <- function
### \emph{x}/\emph{basis}, in percent
(
  x, 
  basis
)
{
  100* x/basis
  ### 100 * x / basis
}

# relativeCumulatedSum ---------------------------------------------------------
relativeCumulatedSum <- function # relativeCumulatedSum
### relative cumulated sum of a vector of values
(
  values
  ### vector of numeric values
) 
{
  cumulated <- cumsum(values)
  maxCumulated <- tail(cumulated, 1)
  100 * cumulated / maxCumulated
}

# colStatistics ----------------------------------------------------------------
colStatistics <- function # colStatistics
### applies statistical functions to all columns of a data frame
(
  dataFrame,
  ### data frame with numeric columns only
  functions = c("sum", "mean", "min", "max", "number.na", "length"),
  ### vector of statistical functions to be applied on each column of dataFrame
  ### possible values: "sum", "mean", "min", "max", "number.na" (number of NA 
  ### values), "length" (number of values)
  na.rm = FALSE,
  ### if TRUE, NA values are removed before applying the statistical function(s)
  functionColumn = FALSE
  ### if TRUE, a column containing the function name is contained in the
  ### result data frame, otherwise the function names become the row names
  ### of the result data frame
)
{
  statistics <- NULL
  
  for (FUN in functions) {
    
    functionStatistics <- colStatisticOneFunction(dataFrame, FUN, na.rm = na.rm)
    
    if (is.null(statistics)) {
      statistics <- functionStatistics
    }
    else {
      statistics <- cbind(statistics, functionStatistics)
    }
  } 
  
  statistics <- t(statistics)
  
  if (functionColumn) {
    rownames(statistics) <- NULL
    data.frame(FUN = functions, t(statistics), stringsAsFactors = FALSE)
  }
  else {
    rownames(statistics) <- functions
    data.frame(t(statistics))
  }  
}

# colStatisticOneFunction ------------------------------------------------------
colStatisticOneFunction <- function # colStatisticOneFunction
### applies a statistical function to all columns of a data frame
(
  dataFrame, 
  FUN,
  ### statistical function to be applied on each column of dataFrame
  ### possible values: "sum", "mean", "min", "max", "number.na" (number of NA 
  ### values), "length" (number of values)  
  na.rm = FALSE
  ### if TRUE, NA values are removed before applying the statistical function  
)
{
  if (FUN == "sum") {
    colSums(dataFrame, na.rm = na.rm)
  } 
  else if (FUN == "mean") {
    colMeans(dataFrame, na.rm = na.rm)
  }
  else if (FUN == "min") {
    colMinima(dataFrame, na.rm = na.rm)
  }
  else if (FUN == "max") {
    colMaxima(dataFrame, na.rm = na.rm)
  }
  else if (FUN == "number.na") {
    colNaNumbers(dataFrame)
  }
  else if (FUN == "length") {
    nrow(dataFrame)
  }  
  else {
    stop("Unknown function '", FUN, 
         "' (must be one of 'sum', 'mean', 'min', 'max', 'number.na', 'length')!")
  }
}

# colMinima --------------------------------------------------------------------
colMinima <- function # colMinima
### minimum per column
(
  dataFrame, 
  na.rm = FALSE
) 
{
  apply(dataFrame, 2, min, na.rm = na.rm)
}

# colMaxima --------------------------------------------------------------------
colMaxima <- function # colMaxima
### maximum per column
(
  dataFrame, 
  na.rm = FALSE
) 
{
  apply(dataFrame, 2, max, na.rm = na.rm)
}

# colNaNumbers -----------------------------------------------------------------
colNaNumbers <- function # colNaNumbers
### number of NA values per column
(
  dataFrame
) 
{
  apply(dataFrame, 2, function(x) {
    sum(is.na(x))
  })
}
