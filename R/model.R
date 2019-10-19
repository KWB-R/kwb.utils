# objectToText -----------------------------------------------------------------
objectToText <- function(object)
{
  catAndRun("Converting object to text", {

    # Create path to a temporary file
    file <- tempfile(fileext = ".txt")

    # Save object to text file
    save(object, file = file, ascii = TRUE)

    # Reread the object as text lines and combine all lines with tab
    paste(readLines(file), collapse = "\t")
  })
}

# textToObject -----------------------------------------------------------------
textToObject <- function(text)
{
  stopifnot(is.character(text), length(text) == 1)

  text_lines <- strsplit(text, "\t")[[1]]

  if (! grepl("^RDA[23]$", text_lines[[1]])) {

    warning(
      "This does not look like something that was stored with ", 
      "objectToText():\n>>>",
      paste(utils::head(text_lines), collapse = "\n"),
      "<<<\nReturning the original text.", 
      call. = FALSE
    )

    return(text)
  }

  catAndRun("Converting text to object", {

    # Create path to a temporary file
    file_path <- tempfile(fileext = ".txt")

    # Open file in binary mode and close file on exit
    con <- file(file_path, "wb")
    on.exit(close(con))

    # Write the text representation of the object to the file using line feed LF
    # which seems to be the line ending used by save(ascii = TRUE)
    write(paste(text_lines, collapse = "\n"), con)

    # Load the object into R
    loadObject(file_path, "object", dbg = FALSE)
  })
}
