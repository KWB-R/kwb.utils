# enumeration ------------------------------------------------------------------
enumeration <- function(x, type = "element", sorted = FALSE, suffix = "")
{
  sprintf(
    "%s%s%s%s:\n- %s\n", 
    type,
    ifelse(length(x) > 1L, "s", ""), # plural "s"
    ifelse(length(x) > 1L && sorted, " (sorted)", ""), 
    suffix,
    stringList(if (sorted) sort(x) else x, collapse = "\n- ")
  )
}

# hintAvailable ----------------------------------------------------------------
hintAvailable <- function(x, type = "element", sorted = TRUE, suffix = "")
{
  paste0("Available ", enumeration(x, type, sorted, suffix))
}

# hintNoSuch -------------------------------------------------------------------
hintNoSuch <- function(x, type = "element", sorted = TRUE, suffix = "")
{
  paste0("No such ", enumeration(x, type, sorted, suffix))
}

# noSuchElements ---------------------------------------------------------------
noSuchElements <- function(
  x, available, type = "element", sorted = TRUE, suffix = ""
)
{
  paste0(
    hintNoSuch(x, type, sorted, suffix),
    hintAvailable(available, type, sorted)
  )
}

# stopIsNotBut -----------------------------------------------------------------
stopIsNotBut <- function(x, expected, name = deparse(substitute(x)))
{
  stop(call. = FALSE, sprintf(
    "%s is not a list but:\n%s",
    name, paste(utils::capture.output(utils::str(x)), collapse = "\n")
  ))
}
