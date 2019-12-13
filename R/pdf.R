# preparePdfIf -----------------------------------------------------------------

#' Prepare PDF File if Condition is Met
#'
#' @param to.pdf condition determining whether \code{\link{preparePdf}} is
#'   called or not
#' @param PDF full path to PDF file. If this is an empty string (default) a
#'   temporary file is created and its path returned
#' @param \dots arguments passed to \code{\link{preparePdf}}
#' @return full path to pdf file created if condition is met or "" else
#' @export
#' 
preparePdfIf <- function(to.pdf, PDF = "", ...)
{
  if (to.pdf) {
    
    PDF <- if (PDF == "") {
      
      preparePdf(...)
      
    } else {
      
      preparePdf(PDF, ...)
    }
  }
  
  PDF
}

# preparePdf -------------------------------------------------------------------

#' Open PDF Device with DIN A4 Dimensions by Default
#'
#' Opens a PDF device in A4 paper format. After calling this function all plots
#' go into the specified PDF file in \code{pdfFile}. Important: The PDF file
#' needs to be closed explicitely with grDevices::dev.off() after all desired
#' plots have been made.
#' 
#' @param pdfFile Full path to PDF file to be created
#' @param landscape 
#'   If \code{TRUE} (default), orientation in PDF file will be landscape, else 
#'   portrait
#' @param borderWidth.cm (Total) border width in "width" direction in cm
#' @param borderHeight.cm (Total) border width in "height" direction in cm
#' @param width.cm page width in cm. Default according to DIN A4
#' @param height.cm page height in cm. Default according to DIN A4
#' @param makeCurrent if \code{TRUE} (default), the opened PDF device will
#'   become the current device, otherwise the current device will be restored
#' @param paper passed to \code{\link[grDevices]{pdf}}. By default "A4" (if
#'   \code{landscape = FALSE}) or "A4r" (if \code{landscape = TRUE}). Use
#'   \code{paper = "special"} to use the dimensions of the plot.
#' @param \dots further arguments passed to \code{\link[grDevices]{pdf}}
#' @return full path to pdf file
#' @seealso \code{\link{finishAndShowPdf}}
#' @export
#' @examples 
#' \dontrun{
#' # Open PDF file by giving a path to preparePdf(). The path is returned.
#' pdf_file <- preparePdf(file.path(tempdir(), "example_preparePdf.pdf"))
#' 
#' # Plot something
#' plot(x <- seq(-pi,pi,pi/100), sin(x), type = "l")
#' 
#' # Open PDF file in viewer
#' finishAndShowPdf(pdf_file)
#' }
#' 
preparePdf <- function(
  pdfFile = tempfile(fileext = ".pdf"), landscape = TRUE, borderWidth.cm = 2, 
  borderHeight.cm = 2, 
  width.cm = .defaultWidth(landscape, borderWidth.cm, borderHeight.cm),
  height.cm = .defaultHeight(landscape, borderWidth.cm, borderHeight.cm),
  makeCurrent = TRUE, paper = paste0("a4", ifelse(landscape, "r", "")), ...
)   
{
  # Save current device
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
  
  # If required, restore old device
  if (! makeCurrent) {
    
    grDevices::dev.set(currentDevice)  
  }
  
  pdfFile
}

# .defaultWidth ----------------------------------------------------------------

.defaultWidth <- function(landscape, borderWidth.cm, borderHeight.cm)
{
  ifelse(landscape, DIN.A4()$height.cm, DIN.A4()$width.cm) - 
    ifelse(landscape, borderWidth.cm, borderHeight.cm)
}

# .defaultHeight ---------------------------------------------------------------

.defaultHeight <- function(landscape, borderWidth.cm, borderHeight.cm)
{
  ifelse(landscape, DIN.A4()$width.cm, DIN.A4()$height.cm) - 
    ifelse(landscape, borderHeight.cm, borderWidth.cm)
}

# DIN.A4 -----------------------------------------------------------------------

#' Width and Height of a DIN A4 Paper
#' 
#' @export
#' 
DIN.A4 <- function()
{
  # DIN A4: 210 x 297 mm, 
  # margins: top = 2.7cm, left = 2.41, right = 2.0, bottom = 2.0  
  
  list(height.cm = 29.7, width.cm = 21.0)
}

# finishAndShowPdfIf -----------------------------------------------------------

#' Finish and Display PDF File if Condition is Met
#' 
#' @param to.pdf if \code{TRUE} the pdf device is closed and the pdf file is
#'   opened in a viewer
#' @param PDF path to the opened pdf file
#' @param ... arguments passed to \code{\link{finishAndShowPdf}}
#' @export
#' 
finishAndShowPdfIf <- function(to.pdf, PDF, ...)   
{
  if (to.pdf) {
    
    finishAndShowPdf(PDF, ...)
  }    
}

# finishAndShowPdf -------------------------------------------------------------

#' Close Device and Open PDF File in Viewer
#' 
#' Close the PDF device (default: \code{dev.cur()}) and open the PDF file in a
#' PDF viewer
#' 
#' @param PDF full path to the PDF file to be opened in the PDF Viewer
#' @param which passed to \code{grDevices::dev.off}
#' @param \dots further arguments passed to \code{hsShowPdf}
#' @export
#' 
finishAndShowPdf <- function(PDF, which = grDevices::dev.cur(), ...)
{
  grDevices::dev.off(which = which)
  
  hsShowPdf(PDF, ...)
}

# hsPrepPdf --------------------------------------------------------------------

#' Prepare Writing of PDF File
#'
#' Deprecated. Please use \code{\link{preparePdf}} instead.
#'
#' @param strPdf see argument \code{pdfFile} in \code{\link{preparePdf}}
#' @param boolLandscape see argument \code{landscape} in \code{\link{preparePdf}}
#' @param bordW see argument \code{borderWidth.cm} in \code{\link{preparePdf}}
#' @param bordH see argument \code{borderHeight.cm} in \code{\link{preparePdf}}
#' @param makeCur see argument \code{makeCurrent} in \code{\link{preparePdf}}
#' @param \dots see \dots in \code{\link{preparePdf}}
#' @export
#' 
hsPrepPdf <- function(
  strPdf = tempfile(fileext = ".pdf"), boolLandscape = TRUE, bordW = 2, 
  bordH = 2, makeCur = TRUE, ...
)
{
  warningDeprecated("hsPrepPdf", "preparePdf")
  
  preparePdf(
    pdfFile = strPdf, 
    landscape = boolLandscape, 
    borderWidth.cm = bordW, 
    borderHeight.cm = bordH, 
    makeCurrent = makeCur, 
    ...
  )
}

# hsShowPdf --------------------------------------------------------------------

#' Open PDF file in PDF viewer
#' 
#' Opens the PDF file of which the full path is given in \emph{Pdf} in a
#'   PDF viewer.
#' 
#' @param Pdf full path to PDF file
#' @param dbg if \code{TRUE} (default) the command used to open the PDF file is
#'   shown
#' @seealso \code{\link{hsPrepPdf}}
#' @export
#' @examples 
#' # Set path to PDF file and open PDF device
#' tmpPdf <- tempfile("ex_hsFinishPdf", fileext = ".pdf") 
#' 
#' hsPrepPdf(tmpPdf)
#' 
#' # Plot something
#' plot(x <- seq(-pi,pi,pi/100), sin(x), type = "l")
#' 
#' # Finish PDF file.
#' grDevices::dev.off()
#' 
#' \dontrun{
#' # Open PDF file in viewer.
#' hsShowPdf(tmpPdf)
#' }
#' 
hsShowPdf <- function(Pdf, dbg = TRUE) 
{
  cmd <- sprintf('"%s" "%s"', getOption("pdfviewer"), Pdf)
  
  if (isTRUE(dbg)) {
    
    hsSystem(cmd)
    
  } else {
    
    system(cmd)
  }
}

#' Let Expressions Plot into a PDF File
#' 
#' The function opens a PDF device with \code{\link{pdf}}, executes the given 
#' expressions, closes the PDF file and displays the file in a PDF viewer.
#' 
#' @param expressions R expressions creating plots that are to be redirected 
#'   into a pdf file. You may pass multiple expressions within opening and 
#'   closing curly braces
#' @param pdfFile optional. Path to the PDF file to be created. The directory
#'   part of the path must exist. If not given or \code{NULL}, the PDF file is
#'   created in the \code{tempdir()} folder.
#' @param \dots further arguments passed to \code{\link{pdf}}
#' @return The function returns the path to the created PDF file.
#' @examples
#' \dontrun{
#' toPdf({
#'   plot(1:10, 10:1)
#'   barplot(1:10)
#'   hist(rnorm(100))
#' })
#' }
toPdf <- function(expressions, pdfFile = NULL, ...)
{
  pdfFile <- defaultIfNULL(pdfFile, tempfile(fileext = ".pdf"))
    
  preparePdf(pdfFile, ...)

  eval(expressions)
  
  finishAndShowPdf(pdfFile)
  
  pdfFile
}
