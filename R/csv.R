# convertCsvFile ---------------------------------------------------------------

#' Modify the format of a CSV file
#' 
#' This function allows you to change the format (such as column delimiter, 
#' decimal character) of a CSV file. It uses \code{\link{read.table}} to read a 
#' CSV file and \code{\link{write.table}} to rewrite the file with modified
#' format to a new file. All arguments of \code{\link{read.table}} and
#' \code{\link{write.table}} are supported. Arguments that are provided by both
#' functions appear as two arguments \code{<argument_name>_in} and 
#' \code{<argument_name>_out} in this function.
#' 
#' @param file_in path to input file
#' @param sep_in column separator in input file
#' @param sep_out column separator in output file
#' @param dec_in decimal character in input file
#' @param dec_out decimal character inoutput file
#' @param file_out path to output file
#' @param header to \code{\link{read.table}}
#' @param quote_in passed as \code{quote} to \code{\link{read.table}}
#' @param quote_out passed as \code{quote} to \code{\link{write.table}}
#' @param row.names_in passed as \code{row.names} to \code{\link{read.table}}
#' @param row.names_out passed as \code{row.names} to \code{\link{write.table}}
#' @param col.names_in passed as \code{col.names} to \code{\link{read.table}}
#' @param col.names_out passed as \code{col.names} to \code{\link{write.table}}
#' @param fileEncoding_in passed as \code{fileEncoding} to
#'   \code{\link{read.table}}
#' @param fileEncoding_out passed as \code{fileEncoding} to
#'   \code{\link{write.table}}
#' @param dbg if \code{TRUE} (default) debug messages are shown
#' @param \dots further arguments passed to either \code{\link{read.table}} or 
#'   \code{\link{write.table}}
#' @return path to the created CSV file   
#' @examples
#' # Write the iris dataset to a temporary file with "," as column separator
#' csv_in <- tempfile(fileext = ".csv")
#' write.table(iris, csv_in, row.names = FALSE)
#' 
#' # Review the first lines of the file
#' catLines(readLines(csv_in, 6))
#' 
#' # Convert the column separator (from " " which was the default) to ";"
#' csv_out <- convertCsvFile(csv_in, sep_out = ";")
#' 
#' # Review the result
#' catLines(readLines(csv_out, 6))
#' 
#' # Delete the file so that it can be recreated
#' unlink(csv_out)
#' 
#' # Convert the column separator and the decimal character
#' csv_out <- convertCsvFile(csv_in, sep_out = ";", dec_out = ",")
#' readLines(csv_out, 6)
convertCsvFile <- function(
  file_in,
  sep_in = formals(utils::read.table)$sep,
  sep_out = sep_in,
  dec_in = formals(utils::read.table)$dec,
  dec_out = dec_in,
  file_out = NULL,
  header = TRUE,
  quote_in = formals(utils::read.table)$quote,
  quote_out = formals(utils::write.table)$quote,
  row.names_in = formals(utils::read.table)$row.names,
  col.names_in = formals(utils::read.table)$col.names,
  row.names_out = FALSE,
  col.names_out = TRUE,
  fileEncoding_in = formals(utils::read.table)$fileEncoding,
  fileEncoding_out = fileEncoding_in,
  dbg = TRUE,
  ...
)
{
  # Put additional arguments into a list
  arguments <- list(...)

  # Set arguments for call of read.table
  args_read <- list(
    file = file_in, header = header, sep = sep_in, quote = quote_in, 
    dec = dec_in, fileEncoding = fileEncoding_in
  )
  
  if (! missing(row.names_in)) {
    args_read <- c(args_read, list(row.names = row.names_in))
  }
  
  if (! missing(col.names_in)) {
    args_read <- c(args_read, list(col.names = col.names_in))
  }
  
  argnames <- setdiff(names(arguments), names(formals(utils::write.table)))
  
  args <- c(args_read, arguments[argnames])

  # Read data from file_in  
  kwb.utils::catIf(dbg, sprintf("Reading from '%s' ... ", file_in))
  x <- do.call(utils::read.table, args)
  kwb.utils::catIf(dbg, "ok.\n")

  # If no target file name is given, create one
  if (is.null(file_out)) {
    
    extension <- kwb.utils::fileExtension(file_in)
    pattern <- paste0("\\.", extension, "$")
    file_out <- gsub(pattern, paste0("_new.", extension), file_in)
  }
  
  # Stop if target file exists
  if (file.exists(file_out)) {
    
    stop("File '", file_out, "' already exists. I will not overwrite it!")
  }
    
  # Set arguments for call of write.table
  args_write <- list(
    x = x, file = file_out, sep = sep_out, quote = quote_out, dec = dec_out, 
    row.names = row.names_out, col.names = col.names_out, 
    fileEncoding = fileEncoding_out
  )
  
  argnames <- setdiff(names(arguments), names(formals(utils::read.table)))
  args <- c(args_write, arguments[argnames])

  # Write data to file_out
  kwb.utils::catIf(dbg, sprintf("Writing to '%s' ... ", file_out))
  do.call(utils::write.table, args)
  kwb.utils::catIf(dbg, "ok.\n")
  
  file_out
}
