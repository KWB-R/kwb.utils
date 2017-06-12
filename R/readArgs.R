# readArglists -----------------------------------------------------------------

#' Read Argument Lists from CSV File
#' 
#' Read argument lists from CSV (or MS Excel) file
#' 
#' @param file full path to comma separated file containing the argument specification
#' @param configTable Instead of the file you may provide a data frame containing the
#'   configuration
#' @param dbg if \code{TRUE}, debug messages are shown
#' 
readArglists <- function # Read Argument Lists from CSV File
### Read argument lists from CSV (or MS Excel) file
(
  file = NULL,
  ### full path to comma separated file containing the argument specification
  configTable = .readArglistsTable.csv(safePath(file), dbg = dbg),
  ### Instead of the file you may provide a data frame containing the
  ### configuration
  dbg = FALSE
  ### if \code{TRUE}, debug messages are shown
)
{
  configs <- lapply(seq_len(nrow(configTable)), function(i) {
    as.list(hsDelEmptyCols(configTable[i, -1]))
  })
  
  names(configs) <- configTable[, 1]
  
  configs <- lapply(configs, .cleanArglistConfig)
  
  # resolve "additional.args"
  indices <- which(sapply(configs, function(x) ! is.null(x$additional.args)))
  
  for (i in indices) {
    configName <- configs[[i]]$additional.args
    stopifnot(is.character(configName))
    configs[[i]] <- arglist(selectElements(configs, configName), configs[[i]][-1])
  }
  
  configs
}

# .readArglistsTable.csv -------------------------------------------------------

#'  readArglistsTable csv
#' 
#' 
.readArglistsTable.csv <- function(file, dbg = FALSE)
{
  configTable <- utils::read.csv(file, stringsAsFactors = FALSE)
  
  # Set empty fields to NA
  for (column in names(configTable)) {
    configTable[, column] <- gsub("^$", NA, configTable[, column])
  }
  
  configTable
}

# .cleanArglistConfig ----------------------------------------------------------

#'  cleanArglistConfig
#' 
#' 
.cleanArglistConfig <- function(config)
{
  lapply(config, function(x) {
    eval(parse(text = gsub("^\\|\\}$", "", x), keep.source = FALSE))
  })
}
