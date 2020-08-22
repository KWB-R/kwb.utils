# createStorage ----------------------------------------------------------------

#' Create Storage Object to Save/Load .rds or .RData files
#' 
#' This function returns an object that implements the methods \code{list()}, 
#' \code{save()}, \code{load()}, \code{remove()} that list, store, load or 
#' remove, respectively .rds or .RData files. The object is associated to a 
#' folder that is given to this function.
#' 
#' @param path path to the folder in which to store the RDS files. If the folder 
#'   does not exist it is attempted to be created. 
#' @param type one of \code{"rds"}, \code{"RData"}, specifying the format used
#'   to store R objects.
#' @return list with "member functions" \code{list()}, \code{save()}, 
#'   \code{load()}, \code{remove()}, and variables \code{path}, \code{type}
#' @export
#' @examples
#' # Create an empty test folder
#' path <- file.path(tempdir(), "test")
#' dir.create(path)
#' 
#' # Create a storage object pointing to a temporary test folder
#' storage <- kwb.utils::createStorage(path)
#' 
#' # At the beginning, the storage is empty
#' storage$list()
#' 
#' # Store objects
#' apple <- 5
#' storage$save(apple, banana = list("Hello", "World"))
#' 
#' \dontrun{
#' # List the objects
#' storage$list()
#' 
#' # Retrieve an object
#' storage$load("banana")
#' 
#' # Remove objects
#' storage$remove("apple")
#' storage$remove("banana")
#' 
#' # The storage is empty again
#' storage$list()
#' }
createStorage <- function(path, type = "rds")
{
  type <- match.arg(type, c("rds", "RData"))
  
  kwb.utils::createDirectory(path)

  object_path <- function(name) {
    file.path(path, paste(name, type, sep = "."))
  }
  
  get_named_arg_assignments <- function() {
    args <- as.list(sys.call(which = 1L)[-1L])
    arg_names <- defaultIfNULL(names(args), character(length(args)))
    unnamed <- arg_names == ""
    arg_names[unnamed] <- lapply(args[unnamed], as.character)
    stats::setNames(lapply(args, eval), arg_names)
  }

  member_list <- function(full.names = FALSE) {
    dir(
      path, 
      pattern = paste0("\\.", type, "$"), 
      ignore.case = TRUE, 
      full.names = full.names, 
      all.files = TRUE
    )
  }

  member_save <- function(..., .objects = NULL, .overwrite = FALSE) {

    if (is.null(.objects)) {
      .objects <- get_named_arg_assignments()
      .objects <- .objects[names(.objects) != ".overwrite"]
      stopifnot(all(nzchar(defaultIfNULL(names(.objects), ""))))
    } 

    for (name in names(.objects)) {

      file <- object_path(name)

      if (file.exists(file)) {

        if (! .overwrite) {
          message(
            "File exists:\n  ", file, "\n",
            "Use .overwrite = TRUE to overwrite. Returning NULL."
          )
          return(NULL)
        }

        message("Overwriting:\n  ", file)
      }

      object <- .objects[[name]]
      
      if (type == "rds") {
        
        saveRDS(object, file = file)
        
      } else {
        
        save(object, file = file)
      }
    }
  }

  member_load <- function(name) {

    stopifnot(is.character(name), length(name) == 1L)
    
    file <- object_path(name)

    if (! file.exists(file)) {
      message("No such file:\n  ", file, ".\nReturning NULL.")
      return(NULL)
    }

    if (type == "rds") {
      
      readRDS(file)
      
    } else {
      
      loadObject(file, objectname = listObjects(file)[[1L]][1L])
    }
  }

  member_remove <- function(name) {

    file <- object_path(name)

    if (! file.exists(file)) {
      message("Nothing to delete. No such file:\n  ", file)
      return(NULL)
    }

    unlink(file)
  }

  list(
    list = member_list,
    save = member_save,
    load = member_load,
    remove = member_remove,
    path = path,
    type = type
  )
}
