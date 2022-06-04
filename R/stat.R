# countOrSum -------------------------------------------------------------------

#' Count or Sum Up Values Within Groups of rows
#'
#' @param x data frame
#' @param by vector of names of columns in \code{x} to be grouped by
#' @param sum.up name of column in \code{x} containing numeric values to be
#'   summed up. If \code{NULL} (default) rows within groups are counted instead
#'   of summing up values within each group
#' @return object of class \code{xtabs} with as many dimensions as there are
#'   values in \code{by}
#' @export
#' @examples
#' # Create a data frame with example data
#' x <- data.frame(
#'   Group = rep(c("A", "B", "C"), 4),
#'   Even = rep(c(FALSE, TRUE), 6),
#'   Value = seq_len(12)
#' )
#'
#' # Count the number of rows for each group
#' countOrSum(x, "Group")
#' countOrSum(x, c("Group", "Even"))
#'
#' # Sum up the values in column "Value" for each group
#' countOrSum(x, "Group", sum.up = "Value")
#' countOrSum(x, c("Group", "Even"), sum.up = "Value")
#' 
countOrSum <- function(x, by = NULL, sum.up = NULL)
{
  checkForMissingColumns(x, c(by, sum.up))
  
  stats::xtabs(toFormula(sum.up, by), x)
}

# countNaInColumn --------------------------------------------------------------

#' Count NA in one Column of a Data Frame
#' 
#' @param data data frame
#' @param column column name
#' @return number of \code{NA} in \code{column} of \code{data}
#' @export
#' 
countNaInColumn <- function(data, column)
{
  sum(is.na(selectColumns(data, column)))
}

# hsMovingMean -----------------------------------------------------------------

#' Moving Mean
#' 
#' Calculate moving mean of \emph{n} values "around" values
#' 
#' @param x vector of values of which moving mean is to be calculated
#' @param n number of values "around" the values in \emph{x}, including the
#'   values in \emph{x}, of which the mean is calculated. Only odd numbers 1, 3,
#'   5, ... allowed. For each x[i] in x the moving mean is calculated by: 
#'   (x[i-(n-1)/2] + ... + x[i-1] + x[i] + x[i+1] + ... + x[i+(n-1)/2]) / n
#' @param na.rm logical. Should missing values (including NaN) be omitted from
#'   the calculations?
#' @return Vector of moving means with the same number of values as there are in
#'   \emph{x}. If na.rm is FALSE, the first \emph{(n-1)/2} values and the last 
#'   \emph{(n-1)/2} values are NA since there are not enough values at the start
#'   and the end of the vector to calculate the mean.
#' @export
#' @examples 
#' x <- rnorm(30)
#' 
#' plot(x, type = "b", main = "Moving mean over 3, 5, 7 points")
#' 
#' times <- 2:4
#' 
#' for (i in times) {
#'   lines(hsMovingMean(x, n = 2*i - 1), col = i, type = "b", lwd =  2)
#' }
#' 
#' legend("topright", fill = times, legend = sprintf("n = %d", 2*times - 1)) 
#' 
hsMovingMean <- function(x, n, na.rm = FALSE)
{
  movingSum(x, n, na.rm) / n
}

#' movingSum
#' 
#' Calculate moving sum of n values "around" values
#' 
#' @param x vector of values of which moving sum is to be calculated
#' @param n number of values "around" the values in \code{x}, including the
#'   values in \code{x}, of which the mean is calculated. Only odd numbers 1, 3,
#'   5, ... allowed. For each x[i] in x the moving sum is calculated by: 
#'   x[i-(n-1)/2] + ... + x[i-1] + x[i] + x[i+1] + ... + x[i+(n-1)/2]
#' @param na.rm logical. Should missing values (including NaN) be omitted from
#'   the calculations?
#' @return Vector of moving sums with the same number of values as there are in
#'   \code{x}. If \code{na.rm} is \code{FALSE}, the first \code{(n-1)/2} values 
#'   and the last \code{(n-1)/2} values are NA since there are not enough values
#'   at the start and at the end of the vector, respectively, to calculate the 
#'   sum.
#' @export
#' @examples 
#' x <- rnorm(30)
#' 
#' plot(x, type = "b", main = "Moving mean over 3, 5, 7 points")
#' 
#' times <- 2:4
#' 
#' for (i in times) {
#' 
#'   lines(movingSum(x, n = 2 * i - 1), col = i, type = "b", lwd =  2)
#' }
#' 
#' legend("topright", fill = times, legend = sprintf("n = %d", 2 * times - 1))
#' 
movingSum <- function(x, n, na.rm = FALSE)
{
  if (! isOddNumber(n)) {
    
    stop(paste(
      "The number n of values to be taken into account for the sum",
      "needs to be an odd number."
    ))
  }
  
  matrix_values <- c(rep(c(x, rep(NA, n)), n - 1), x)
  
  m <- matrix(matrix_values, ncol = n)
  
  sums <- rowSums(m, na.rm = na.rm)
  
  n_remove <- (n - 1) / 2
  
  if (n_remove > 0) {
    
    i_at_begin <- seq(1, by = 1, length.out = n_remove)
    
    i_at_end <- seq(nrow(m), by = -1, length.out = n_remove)
    
    sums <- sums[-c(i_at_begin, i_at_end)]
  }
  
  sums
}

# percentageOfMaximum ----------------------------------------------------------

#' Percentage of Maximum
#' 
#' @param x vector of numeric values
#' @param na.rm passed to \code{max}
#' @return 100 * x / max(x)
#' @export
#' 
percentageOfMaximum <- function(x, na.rm = TRUE)
{
  percentage(x, max(x, na.rm = na.rm))
}

# percentageOfSum --------------------------------------------------------------

#' Percentage of the Sum of Values
#' 
#' @param x vector of numeric values
#' @param na.rm passed to \code{max}
#' @return 100 * x / sum(x)
#' @export
#' @examples 
#' p <- percentageOfSum(1:10)
#' stopifnot(sum(p) == 100)
#' 
percentageOfSum <- function(x, na.rm = TRUE)
{
  percentage(x, sum(x, na.rm = na.rm))
}

# percentage -------------------------------------------------------------------

#' Percentage
#' 
#' \code{x / basis}, in percent
#' 
#' @param x numeric 
#' @param basis numeric
#' @return 100 * x / basis
#' @export
#' 
percentage <- function(x, basis)
{
  100 * x/basis
}

# relativeCumulatedSum ---------------------------------------------------------

#' Relative Cumulated Sum
#' 
#' relative cumulated sum of a vector of values
#' 
#' @param values vector of numeric values
#' @export
#' 
relativeCumulatedSum <- function(values) 
{
  cumulated <- cumsum(values)
  
  maxCumulated <- lastElement(cumulated)
  
  100 * cumulated / maxCumulated
}

# columnwisePercentage ---------------------------------------------------------

#' Columnwise Percentage
#' 
#' Calculate the percentage (value divided by sum of values in the column) for 
#' each column
#' 
#' @param x two dimensional numeric data structure
#' @param default default value to be used if the calculated percentage is 
#'   \code{NA}.
#' @param digits number of digits (default: 1) to which the resulting 
#'   percentages are to be rounded. Set to \code{NA} to suppress rounding
#' @export
#' @examples 
#' # Create a random matrix of integer values
#' M1 <- matrix(sample(100, 12), nrow = 4, dimnames = list(LETTERS[1:4], 1:3))
#' 
#' # Introduce some NA
#' values <- as.numeric(M1)
#' values[sample(length(values), 3)] <- NA
#' M2 <- matrix(values, nrow = nrow(M1), dimnames = dimnames(M1))
#' 
#' M1
#' columnwisePercentage(M1)
#' 
#' M2
#' columnwisePercentage(M2)
#' columnwisePercentage(M2, default = 0)
#' 
columnwisePercentage <- function(x, default = 0, digits = 1)
{
  rowOrColumnwisePercentage(x, rowwise = FALSE, default, digits)
}

# rowOrColumnwisePercentage ----------------------------------------------------

#' Rowwise or Columnwise Percentage
#' 
#' Calculate the percentage (value divided by sum of values in the row/column) 
#' for each row/column
#' 
#' @param x two dimensional numeric data structure
#' @param rowwise if \code{TRUE} the percentage is calculated by row, else by
#'   column
#' @param default default value to be used if the calculated percentage is 
#'   \code{NA}.
#' @param digits number of digits (default: 1) to which the resulting 
#'   percentages are to be rounded. Set to \code{NA} to suppress rounding
#' @export
#' @seealso \code{\link{rowwisePercentage}}, \code{\link{columnwisePercentage}} 
#' 
rowOrColumnwisePercentage <- function(x, rowwise, default = 0, digits = 1)
{
  stopifnot(length(dim(x)) == 2)
  
  # Copy row and column names from the input x
  fractions <- if (rowwise) {
    
    t(apply(x, 1, percentageOfSum))
    
  } else {
    
    apply(x, 2, percentageOfSum)
  }
  
  fractions[is.na(fractions)] <- default

  dimnames(fractions) <- dimnames(x)
  
  if (! is.na(digits)) {
    
    round(fractions, digits) 
    
  } else {
    
    fractions
  }
}

# rowwisePercentage ------------------------------------------------------------

#' Rowwise Percentage
#' 
#' Calculate the percentage (value divided by sum of values in the row) for 
#' each row
#' 
#' @param x two dimensional numeric data structure
#' @param default default value to be used if the calculated percentage is 
#'   \code{NA}.
#' @param digits number of digits (default: 1) to which the resulting 
#'   percentages are to be rounded. Set to \code{NA} to suppress rounding
#' @export
#' @examples 
#' # Create a random matrix of integer values
#' M1 <- matrix(sample(100, 12), nrow = 4, dimnames = list(LETTERS[1:4], 1:3))
#' 
#' # Introduce some NA
#' values <- as.numeric(M1)
#' values[sample(length(values), 3)] <- NA
#' M2 <- matrix(values, nrow = nrow(M1), dimnames = dimnames(M1))
#' 
#' M1
#' rowwisePercentage(M1)
#' 
#' M2
#' rowwisePercentage(M2)
#' rowwisePercentage(M2, default = 0)
#' 
rowwisePercentage <- function(x, default = 0, digits = 1)
{
  rowOrColumnwisePercentage(x, rowwise = TRUE, default, digits)
}

# colStatistics ----------------------------------------------------------------

#' Column Statistics
#' 
#' applies statistical functions to all columns of a data frame
#' 
#' @param dataFrame data frame with numeric columns only
#' @param functions vector of statistical functions to be applied on each column
#'   of dataFrame possible values: "sum", "mean", "min", "max", "number.na"
#'   (number of NA values), "length" (number of values)
#' @param na.rm if TRUE, NA values are removed before applying the statistical
#'   function(s)
#' @param functionColumn if TRUE, a column containing the function name is
#'   contained in the result data frame, otherwise the function names become the
#'   row names of the result data frame
#' @export
#' 
colStatistics <- function(
  dataFrame, 
  functions = c("sum", "mean", "min", "max", "number.na", "length"),
  na.rm = FALSE, 
  functionColumn = FALSE
)
{
  statistics <- NULL
  
  for (FUN in functions) {
    
    functionStatistics <- colStatisticOneFunction(dataFrame, FUN, na.rm = na.rm)
    
    statistics <- if (is.null(statistics)) {
      functionStatistics
    } else {
      cbind(statistics, functionStatistics)
    }
  } 
  
  statistics <- t(statistics)
  
  if (functionColumn) {
    
    rownames(statistics) <- NULL
    data.frame(FUN = functions, statistics, stringsAsFactors = FALSE)
    
  } else {
    
    rownames(statistics) <- functions
    data.frame(t(statistics))
  }  
}

# colStatisticOneFunction ------------------------------------------------------

#' Apply Function to All Columns
#' 
#' Applies a statistical function to all columns of a data frame
#' 
#' @param dataFrame a data frame of which statistics are to be calculated
#' @param FUN statistical function to be applied on each column of dataFrame 
#'   possible values: "sum", "mean", "min", "max", "number.na" (number of NA 
#'   values), "length" (number of values)
#' @param na.rm if TRUE, NA values are removed before applying the statistical
#'   function
#' @export
#' 
colStatisticOneFunction <- function(dataFrame, FUN, na.rm = FALSE)
{
  if (FUN == "sum") {
    
    colSums(dataFrame, na.rm = na.rm)

  } else if (FUN == "mean") {
    
    colMeans(dataFrame, na.rm = na.rm)
    
  } else if (FUN == "min") {
    
    colMinima(dataFrame, na.rm = na.rm)
    
  } else if (FUN == "max") {
    
    colMaxima(dataFrame, na.rm = na.rm)
    
  } else if (FUN == "number.na") {
    
    colNaNumbers(dataFrame)
    
  } else if (FUN == "length") {
    
    nrow(dataFrame)
    
  } else {
    
    stop(
      "Unknown function '", FUN, 
      "' (must be one of 'sum', 'mean', 'min', 'max', 'number.na', 'length')!"
    )
  }
}

# colMinima --------------------------------------------------------------------

#' Columnwise Minima
#' 
#' Calculate the minima within each column
#' 
#' @param dataFrame data frame of which to calculate columnwise minima
#' @param na.rm passed to the \code{min} function
#' @export
#' 
colMinima <- function(dataFrame, na.rm = FALSE) 
{
  apply(dataFrame, 2, min, na.rm = na.rm)
}

# colMaxima --------------------------------------------------------------------

#' Columnwise Maxima
#' 
#' Calculate the maxima within each column
#' 
#' @param dataFrame data frame of which to calculate columnwise maxima
#' @param na.rm passed to the \code{max} function
#' @export
#' 
colMaxima <- function(dataFrame, na.rm = FALSE) 
{
  apply(dataFrame, 2, max, na.rm = na.rm)
}

# colNaNumbers -----------------------------------------------------------------

#' Columnwise Number of NA 
#' 
#' Calculate the number of NA values within each column
#' 
#' @param dataFrame data frame of which to calculate columnwise NA values
#' @export
#' 
colNaNumbers <- function(dataFrame)
{
  apply(dataFrame, 2, function(x) sum(is.na(x)))
}
