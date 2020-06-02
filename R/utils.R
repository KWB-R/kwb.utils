# backspace --------------------------------------------------------------------

#' String of n Backspaces
#' 
#' @param n number of backspace characters
#' @return vector of character of length one
#' @export
#' @examples
#' update <- function(x) cat(backspace(3), x)
#' x <- "value: 123"
#' cat(x)
#' cat(paste0(x, backspace(3), "987"))
#' 
backspace <- function(n)
{
  repeated("\b", n)
}

# left -------------------------------------------------------------------------

#' Left Part of a String
#' 
#' @param x vector of character
#' @param n number of characters to be kept from the beginning of each character
#'   string within \code{x}
#' @return vector of character
#' @export
#' @examples
#' left("Good Morning", 4)
#' 
left <- function(x, n)
{
  substr(x, 1L, n)
}

# orderBy ----------------------------------------------------------------------

#' Order a Data Frame by One or more Columns
#' 
#' @param df data frame
#' @param by vector of column names specifying the columns by which to order
#' @param \dots further arguments passed to \code{\link{order}}, such as
#'   \code{decreasing}
#' @return \code{df} being sorted and with newly renumbered rows
#' @export
#' @examples
#' df <- data.frame(number = 4:1, letter = LETTERS[1:4])
#' df
#' orderBy(df, "number")
#' orderBy(df, "letter", decreasing = TRUE)
#' 
orderBy <- function(df, by = NULL, ...)
{
  kwb.utils::resetRowNames(
    df[order(kwb.utils::selectColumns(df, by), ...), , drop = FALSE]
  )
}

# repeated ---------------------------------------------------------------------

#' Repeated Substring
#' 
#' @param x substring to be repeated and pasted together to a new string
#' @param n number of times to repeat the substring
#' @return vector of character of length one
#' @export
#' @examples 
#' repeated("no ", 2)
#' repeated("yes ", 3)
#' repeated("yes no ", 3)
#' 
repeated <- function(x, n)
{
  paste(rep(x, n), collapse = "")
}

# right ------------------------------------------------------------------------

#' Right Part of a String
#' 
#' @param x vector of character
#' @param n number of characters to be kept from the end of each character 
#'   string within \code{x}
#' @return vector of character
#' @export
#' @examples
#' right("Good Morning", 7)
#' 
right <- function(x, n)
{
  nc <- nchar(x)
  substr(x, nc - n + 1L, nc)
}

# space ------------------------------------------------------------------------

#' Space String Used for Indentation
#' 
#' Chain together \code{depth * tabLength} spaces
#' 
#' @param depth depth of indentation
#' @param tabLength number of spaces per indentation level
#' @return vector of character of length one consisting of \code{depth *
#'   tabLength} space characters
#' @export
#' @examples
#' cat(sprintf("%s1\n%s2\n%s3\n", space(1), space(2), space(3)))
#' cat(sprintf("%s1\n%s2\n%s3\n", space(1, 4), space(2, 4), space(3, 4)))
#' 
space <- function(depth = 1L, tabLength = 2L)
{
  repeated(" ", depth * tabLength)
}
