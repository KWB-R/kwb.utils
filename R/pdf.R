# preparePdfIf -----------------------------------------------------------------

#' preparePdfIf
#' 
#' prepare pdf file if condition is met
#'
#' @param to.pdf condition determining whether \code{\link{preparePdf}} is
#'   called or not
#' @param PDF full path to PDF file. If this is an empty string (default) a
#'   temporary file is created and its path returned
#' @param \dots arguments passed to \code{\link{preparePdf}}
#' 
#' @return full path to pdf file created if condition is met or "" else
#' 
preparePdfIf <- function(to.pdf, PDF = "", ...)
{
  if (to.pdf) {
    
    if (PDF == "") {
      PDF <- preparePdf(...)
    }
    else {
      PDF <- preparePdf(PDF, ...)
    }
  }
  
  return (PDF)
}

# preparePdf -------------------------------------------------------------------

#' Open PDF Device with DIN A4 Dimensions
#' 
#' @param pdfFile Full path to PDF file to be created
#' @param landscape If TRUE (default), orientation in PDF file will be
#'   landscape, else portrait
#' @param borderWidth.cm (Total) border width in "width" direction in cm
#' @param borderHeight.cm (Total) border width in "height" direction in cm
#' @param width.cm page width in cm. Default according to DIN A4
#' @param height.cm page height in cm. Default according to DIN A4
#' @param makeCurrent if TRUE (default), the opened PDF device will become the
#'   current device
#' @param paper passed to \code{\link[grDevices]{pdf}}. By default "A4" (if
#'   \code{landscape = FALSE}) or "A4r" (if \code{landscape = TRUE}). Use
#'   \code{paper = "special"} to use the dimensions of the plot.
#' @param \dots further arguments passed to \code{\link[grDevices]{pdf}}
#' 
#' @return full path to pdf file
#' 
preparePdf <- function # open PDF device with DIN A4 dimensions
### open PDF device with DIN A4 dimensions
(
  pdfFile = tempfile(fileext = ".pdf"), 
  landscape = TRUE, 
  borderWidth.cm = 2, 
  borderHeight.cm = 2,
  width.cm = .defaultWidth(landscape, borderWidth.cm, borderHeight.cm),
  height.cm = .defaultHeight(landscape, borderWidth.cm, borderHeight.cm),
  makeCurrent = TRUE,
  paper = paste0("a4", ifelse(landscape, "r", "")),
  ...
)   
{
  ## save current device
  currentDevice <- grDevices::dev.cur()
  
  # Open PDF graphics device
  grDevices::pdf(
    file = pdfFile, 
    # "r" = rotated (landscape)
    paper = paper, 
    width = toInches(width.cm), 
    height = toInches(height.cm), 
    ...
  )
  
  ## if required, restore old device
  if (! makeCurrent) {
    grDevices::dev.set(currentDevice)  
  }
  
  pdfFile
  ### full path to pdf file
}

# .defaultWidth ----------------------------------------------------------------

#'  defaultWidth
#' 
#' 
.defaultWidth <- function(landscape, borderWidth.cm, borderHeight.cm)
{
  ifelse(landscape, DIN.A4()$height.cm, DIN.A4()$width.cm) - 
    ifelse(landscape, borderWidth.cm, borderHeight.cm)
}

# .defaultHeight ---------------------------------------------------------------

#'  defaultHeight
#' 
#' 
.defaultHeight <- function(landscape, borderWidth.cm, borderHeight.cm)
{
  ifelse(landscape, DIN.A4()$width.cm, DIN.A4()$height.cm) - 
    ifelse(landscape, borderHeight.cm, borderWidth.cm)
}

# DIN.A4 -----------------------------------------------------------------------

#' width and height of a DIN A4 paper
#' 
#' width and height of a DIN A4 paper
#' 
DIN.A4 <- function # width and height of a DIN A4 paper
### width and height of a DIN A4 paper
(
)
{
  # DIN A4: 210 x 297 mm, 
  # margins: top = 2.7cm, left = 2.41, right = 2.0, bottom = 2.0  
  list(height.cm = 29.7, width.cm = 21.0)
}

# finishAndShowPdfIf -----------------------------------------------------------

#' finishAndShowPdfIf
#' 
#' finish and display pdf file if condition is met
#' 
#' @param to.pdf if \code{TRUE} the pdf device is closed and the pdf file is
#'   opened in a viewer
#' @param PDF path to the opened pdf file
#' @param ... arguments passed to \code{\link{finishAndShowPdf}}
finishAndShowPdfIf <- function # finishAndShowPdfIf
### finish and display pdf file if condition is met
(
  to.pdf, 
  PDF, 
  ...
)   
{
  if (to.pdf) {
    finishAndShowPdf(PDF, ...)
  }    
}

# finishAndShowPdf -------------------------------------------------------------

#' close device and open PDF file in viewer
#' 
#' close the PDF device (default: \code{dev.cur()}) and open the PDF file in a
#'   PDF viewer
#' 
#' @param PDF full path to the PDF file to be opened in 
#' @param which passed to \code{grDevices::dev.off}
#' @param \dots further arguments passed to \code{hsShowPdf}
#' 
finishAndShowPdf <- function # close device and open PDF file in viewer
### close the PDF device (default: \code{dev.cur()}) and open the PDF file in a
### PDF viewer
(
  PDF, 
  ### full path to the PDF file to be opened in 
  which = grDevices::dev.cur(),
  ### passed to \code{grDevices::dev.off}
  ...
  ### further arguments passed to \code{hsShowPdf}
)   
{
  grDevices::dev.off(which = which)
  
  hsShowPdf(PDF, ...)
}

# hsPrepPdf --------------------------------------------------------------------

#' Prepare writing of PDF file
#' 
#' Opens a PDF device in A4 paper format. After calling this function all
#'   plots go into the specified PDF file in \eqn{strPdf}. Important: The PDF
#'   file needs to be closed explicitely with grDevices::dev.off() after all
#'   desired plots have been made.
#' 
#' @param strPdf Full path to PDF file to be created
#' @param boolLandscape If TRUE, orientation in PDF file will be landscape, else portrait
#' @param bordW (Total) border width in "width" direction in cm
#' @param bordH (Total) border width in "height" direction in cm
#' @param makeCur if TRUE, the new pdf device will become the current device, otherwise
#'   the current device will be restored
#' @param \dots further arguments passed to \code{pdf}
#' 
#' @seealso \code{\link{hsShowPdf}}
#' 
#' @examples 
#'   
#'   # Set path to PDF file and open PDF device
#'   pdfFile <- file.path(tempdir(), "ex_hsPrepPdf.pdf")
#'   hsPrepPdf(pdfFile)
#'   
#'   ## Plot something
#'   plot(x <- seq(-pi,pi,pi/100), sin(x), type = "l")
#'   
#'   ## Close PDF device
#'   grDevices::dev.off()
#'   
#'   ## Open PDF file in viewer
#'   hsShowPdf(pdfFile)
#'   
#' 
hsPrepPdf <- structure(
  function # Prepare writing of PDF file
  ### Opens a PDF device in A4 paper format. After calling this function all
  ### plots go into the specified PDF file in \eqn{strPdf}. Important: The PDF
  ### file needs to be closed explicitely with grDevices::dev.off() after all
  ### desired plots have been made.
  (
    strPdf = tempfile(fileext = ".pdf"), 
    ### Full path to PDF file to be created
    boolLandscape = TRUE, 
    ### If TRUE, orientation in PDF file will be landscape, else portrait
    bordW = 2, 
    ### (Total) border width in "width" direction in cm
    bordH = 2,
    ### (Total) border width in "height" direction in cm
    makeCur = TRUE,
    ### if TRUE, the new pdf device will become the current device, otherwise
    ### the current device will be restored
    ...
    ### further arguments passed to \code{pdf}
  ) 
  {
    ##seealso<< \code{\link{hsShowPdf}}
    
    warning("Please use preparePdf instead of hsPrepPdf!")
    
    preparePdf(
      pdfFile = strPdf, 
      landscape = boolLandscape, 
      borderWidth.cm = bordW, 
      borderHeight.cm = bordH, 
      makeCurrent = makeCur, 
      ...
    )
  },
  ex = function() {
    
    # Set path to PDF file and open PDF device
    pdfFile <- file.path(tempdir(), "ex_hsPrepPdf.pdf")
    hsPrepPdf(pdfFile)
    
    ## Plot something
    plot(x <- seq(-pi,pi,pi/100), sin(x), type = "l")
    
    ## Close PDF device
    grDevices::dev.off()
    
    ## Open PDF file in viewer
    hsShowPdf(pdfFile)
  })

# hsShowPdf --------------------------------------------------------------------

#' Open PDF file in PDF viewer
#' 
#' Opens the PDF file of which the full path is given in \emph{Pdf} in a
#'   PDF viewer.
#' 
#' @param Pdf full path to PDF file
#' @param dbg if \code{TRUE} (default) the command used to open the PDF file is
#'   shown
#' 
#' @seealso \code{\link{hsPrepPdf}}
#' 
#' @examples 
#'   
#'   # Set path to PDF file and open PDF device
#'   tmpPdf <- tempfile("ex_hsFinishPdf", fileext = ".pdf") 
#'   hsPrepPdf(tmpPdf)
#'   
#'   ## Plot something
#'   plot(x <- seq(-pi,pi,pi/100), sin(x), type = "l")
#'   
#'   ## Finish PDF file.
#'   grDevices::dev.off()
#'   
#'   ## Open PDF file in viewer.
#'   hsShowPdf(tmpPdf)
#'   
hsShowPdf <- function(Pdf, dbg = TRUE) 
{
  cmd <- sprintf('"%s" "%s"', getOption("pdfviewer"), Pdf)
  
  catIf(dbg, "cmd:", cmd, "\n")
  
  system(cmd)
}
