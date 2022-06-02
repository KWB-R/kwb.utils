#' Order Data Frame Decreasingly by one Column
#'
#' @param df data frame
#' @param column name of column by which to order decreasingly.
#' @export
#' @examples
#' (df <- data.frame(a = 1:3, b = 11:13))
#' orderDecreasinglyBy(df, "a")
orderDecreasinglyBy <- function(df, column)
{
  # Order decreasingly by this "effective" score
  resetRowNames(df[order(selectColumns(df, column), decreasing = TRUE), ])
}
