\name{resolveAll}
\alias{resolveAll}
\title{resolve all placeholders in a dictionary}
\description{resolve all placeholders in a dictionary}
\usage{resolveAll(dictionary)}
\arguments{
  \item{dictionary}{list with named elements where the element name represents the key and the
element value represents the value assigned to the key.}
}



\author{Hauke Sonnenberg}




\examples{
dictionary <- list(
  basedir = "C:/myNicefolder",
  projectdir = "<basedir>/projects/<projectName>",
  inputdir = "<projectdir>/input",
  outputdir = "<projectdir>/output"
)

dictionary$projectName <- "project1"
dictionary1 <- resolveAll(dictionary)

dictionary$projectName <- "project2"
dictionary2 <- resolveAll(dictionary)

dictionary1$input
dictionary1$output    

dictionary2$inputdir
dictionary2$output    
}
