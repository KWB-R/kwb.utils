\name{getNamesOfObjectsInRDataFiles}
\alias{getNamesOfObjectsInRDataFiles}
\title{get names of objects in .RData files}
\description{get names of objects in .RData files}
\usage{getNamesOfObjectsInRDataFiles(files.rdata)}
\arguments{
  \item{files.rdata}{vector of full paths to .RData files}
}



\author{Hauke Sonnenberg}




\examples{
## Not run

## Search for available .RData files below "searchdir"
#searchdir <- "//poseidon/projekte$/SUW_Department/Projects/SEMA/WP/20_Braunschweig"  
#files.rdata <- dir(searchdir, pattern = "\\\\.RData$", recursive = TRUE, full.names = TRUE)

## Get the names of the objects in the .RData files
#objectsInFiles <- getNamesOfObjectsInRDataFiles(files.rdata = files.rdata)

## Which file contains the object "DataQ"?
#dataQ.found <- sapply(objectsInFiles, function(x) {"DataQ" \%in\% x})

#cat("DataQ was found in the following files:", 
#    paste(files.rdata[dataQ.found], collapse = "\n  "))  
}
