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
get_cached <- function(name, dbg = TRUE)
{
  file <- get_cached_file(name)
  
  if (! file.exists(file)) {
    return(NULL)
  }
  
  loadObject(file, "x", dbg = dbg)
}

# get_cached_file --------------------------------------------------------------
get_cached_file <- function(name = "")
{
  cache_dir <- get_cache_dir()

  createDirectory(cache_dir, dbg = FALSE)
  
  if (name == "") {
    return(dir(cache_dir, "\\.RData", full.names = TRUE))
  }
  
  file.path(cache_dir, paste0(name, ".RData"))
}

# get_cache_dir ----------------------------------------------------------------
get_cache_dir <- function()
{
  Sys.getenv("KWB_UTILS_CACHE_DIR", file.path(tempdir(), "cache"))
}

# set_cache_dir ----------------------------------------------------------------
set_cache_dir <- function(path)
{
  Sys.setenv(KWB_UTILS_CACHE_DIR = path)
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
