#' Create Named Vector from two Columns of a Data Frame
#' 
#' @param data a data frame
#' @param valueColumn name of column in \code{data} containing the names
#' @param nameColumn name of column in \code{data} containing the values
#' @return vector with values in \code{data[[valueColumn]]}, named by
#'   \code{data[[nameColumn]]}
#' @export
#' @examples
#' data <- data.frame(a = 1:3, name = LETTERS[1:3])
#' namedVectorFromColumns(data, valueColumn = "a", nameColumn = "name")
namedVectorFromColumns <- function(data, valueColumn, nameColumn)
{
  stats::setNames(
    object = selectColumns(data, valueColumn),
    nm = selectColumns(data, nameColumn)
  )
}
