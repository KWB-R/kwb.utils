# encode -----------------------------------------------------------------------

#' Encode a Vector of Character
#' 
#' @param x vector of character
#' @param level one of 1, 2, or 3 defining which character set to use for 
#'   encoding, see \code{\link{printable_chars}}.
#' @param chars vector of characters to be used for encoding
#' 
#' @return vector of character as long as \code{x} with each element containing
#'   the encoded version of the corresponding element in \code{x}. The returned
#'   vector has an attribute \code{codes} being a named vector. This vector 
#'   contains the unique values in \code{x} as elements. Each element is named
#'   by the code that was used to encode the corresponding element.
#' 
#' @examples 
#' x <- c("very-long-word", "very-long-word", "very-very-long-word")
#' encoded <- encode(x)
#' encoded
#' decode(encoded)
#' 
encode <- function(x, level = 1, chars = printable_chars(level))
{
  x <- as.factor(x)
  
  m <- kwb.utils::intToNumeralSystem(seq_along(levels(x)), base = length(chars))
  
  result <- matrix(
    chars[m + 1], 
    nrow = nrow(m), 
    ncol = ncol(m), 
    dimnames = dimnames(m)
  ) 
  
  result <- do.call(paste0, kwb.utils::asColumnList(result))
  
  encoded_indices <- gsub(pattern = "^0+", replacement = "", result)
  
  index_codes <- stats::setNames(encoded_indices, rownames(m))
  
  structure(
    unname(index_codes[as.integer(x)]), 
    codes = stats::setNames(levels(x), as.character(index_codes))
  )
}

# printable_chars --------------------------------------------------------------

#' Different Sets of Printable ASCII Characters
#' 
#' @param level one of 1, 2, or 3. Level 1 characters comprise the ten digits 0 
#'   to 9 and 26 uppercase letters. Level 2 characters comprise the characters 
#'   of level 1 as well as 26 lowercase letters. Level 3 characters comprise
#'   altogether 88 printable characters. 
#'   
#' @return vector of character
#' 
#' @examples 
#' printable_chars(1)
#' printable_chars(2)
#' printable_chars(3)
#' 
printable_chars <- function(level = 1)
{
  DIGITS <- as.character(0:9)
  
  if (level == 1) {
    
    c(DIGITS, LETTERS)
    
  } else if (level == 2) {
    
    c(DIGITS, LETTERS, letters)
    
  } else if (level == 3) {
    
    printable <- strsplit(rawToChar(as.raw(33:126)), "")[[1]]
    
    special <- c(",", ";", "`", '"', "'", "\\")
    
    c(DIGITS, setdiff(printable, c(special, DIGITS)))
    
  } else {
    
    stop(call. = FALSE, "Unexpected level: ", level, ". Expected: 1, 2 or 3.")
  }
}

# decode -----------------------------------------------------------------------

#' Decode a Vector of Character
#' 
#' For an example see \code{\link{encode}}.
#' 
#' @param x vector of encoded character strings as returned by 
#'   \code{\link{encode}}
#'   
#' @return vector of decoded character strings
#' 
decode <- function(x)
{
  unname(kwb.utils::getAttribute(x, "codes")[x])
}
