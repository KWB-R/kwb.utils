# back -------------------------------------------------------------------------
back <- function(n)
{
  repeated("\b", n)
}

# left -------------------------------------------------------------------------
left <- function(x, n)
{
  substr(x, 1L, n)
}

# orderBy ----------------------------------------------------------------------
orderBy <- function(x, by = NULL)
{
  kwb.utils::resetRowNames(
    x[order(kwb.utils::selectColumns(x, by)), , drop = FALSE]
  )
}

# repeated ---------------------------------------------------------------------
repeated <- function(x, n)
{
  paste(rep(x, n), collapse = "")
}

# right ------------------------------------------------------------------------
right <- function(x, n)
{
  nc <- nchar(x)
  substr(x, nc - n + 1L, nc)
}

# space ------------------------------------------------------------------------
space <- function(depth = 1L)
{
  repeated("  ", depth)
}
