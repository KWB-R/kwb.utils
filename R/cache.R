# cache_and_return -------------------------------------------------------------
cache_and_return <- function(x, name = deparse(substitute(x)))
{
  save(x, file = get_cached_file(name))
  x
}

# clear_cache ------------------------------------------------------------------
clear_cache <- function()
{
  files <- get_cached_file()
  
  if (length(files)) {
    
    catAndRun(messageText = sprintf(
      "Clearing %d files from cache:\n  %s", 
      length(files), stringList(basename(files))
    ), expr = {
      unlink(files)
    })
    
  } else {
    
    cat("No cached files to clear.\n")
  }
}

# get_cached -------------------------------------------------------------------
get_cached <- function(name)
{
  if (file.exists(file <- get_cached_file(name))) {
    loadObject(file, "x")
  } else {
    NULL
  }
}

# get_cached_file --------------------------------------------------------------
get_cached_file <- function(name = "")
{
  cache_dir <- file.path(tempdir(), "cache")
  
  createDirectory(cache_dir, dbg = FALSE)
  
  if (name == "") {
    return(dir(cache_dir, "\\.RData", full.names = TRUE))
  }
  
  file.path(cache_dir, paste0(name, ".RData"))
}

# run_cached -------------------------------------------------------------------
run_cached <- function(name, expr)
{
  if (! is.null(object <- get_cached(name))) {
    return(object)
  }
  
  object <- eval(expr, envir = -1)
  
  cache_and_return(object, name = name)
  
  object
}
