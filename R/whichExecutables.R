#' Find Executable Files in Directories given in PATH Environment Variable
#' 
#' @param pattern if not NULL, the result list is filtered for file names 
#'   matching the pattern
#' @param full.names if TRUE, the list items are the full paths, otherwise the
#'   file names only
#' @return list with one entry per directory from the PATH variable
#' @export
whichExecutables <- function(pattern = NULL, full.names = FALSE)
{
  paths <- kwb.utils::rStylePath(strsplit(Sys.getenv("PATH"), ";")[[1]])
  extensions <- strsplit(Sys.getenv("PATHEXT"), ";")[[1L]]
  ext_patterns <- gsub("\\.", "\\\\.", extensions)
  ext_pattern <- sprintf("(%s)$", paste(ext_patterns, collapse = "|"))
  exe_files <- lapply(
    stats::setNames(nm = paths), 
    FUN = dir, 
    pattern = ext_pattern, 
    ignore.case = TRUE
  )
  if (!is.null(pattern)) {
    exe_files <- lapply(exe_files, grep, pattern = pattern, value = TRUE)
    exe_files <- exe_files[lengths(exe_files) > 0L]
  }
  if (full.names) {
    exe_files <- lapply(stats::setNames(nm = names(exe_files)), function(path) {
      file.path(path, exe_files[[path]])
    })  
  }
  exe_files
}
