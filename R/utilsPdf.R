# preparePdfIf -----------------------------------------------------------------
preparePdfIf <- function # preparePdfIf
### prepare pdf file if condition is met
(
  to.pdf, 
  PDF = "", 
  ...
)   
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
  ### full path to pdf file created if condition is met or "" else
}

# preparePdf -------------------------------------------------------------------
preparePdf <- function # open PDF device with DIN A4 dimensions
### open PDF device with DIN A4 dimensions
(
  pdfFile = tempfile(fileext = ".pdf"), 
  ### Full path to PDF file to be created
  landscape = TRUE, 
  ### If TRUE (default), orientation in PDF file will be landscape, else
  ### portrait  
  borderWidth.cm = 2, 
  ### (Total) border width in "width" direction in cm
  borderHeight.cm = 2,
  ### (Total) border width in "height" direction in cm
  width.cm = .defaultWidth(landscape, borderWidth.cm, borderHeight.cm),
  ### page width in cm. Default according to DIN A4
  height.cm = .defaultHeight(landscape, borderWidth.cm, borderHeight.cm),
  ### page height in cm. Default according to DIN A4
  makeCurrent = TRUE,
  ### if TRUE (default), the opened PDF device will become the current device
  ...
  ### further arguments passed to \code{pdf}
)   
{
  ## save current device
  currentDevice <- dev.cur()
  
  # Open PDF graphics device
  pdf(
    file = pdfFile, 
    # "r" = rotated (landscape)
    paper = paste0("a4", ifelse(landscape, "r", "")), 
    width = toInches(width.cm), 
    height = toInches(height.cm), 
    ...
  )
  
  ## if required, restore old device
  if (! makeCurrent) {
    dev.set(currentDevice)  
  }
  
  pdfFile
  ### full path to pdf file
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
finishAndShowPdf <- function # finishAndShowPdf
### finish and display pdf file
(
  PDF, 
  ...
)   
{
  dev.off()
  hsShowPdf(PDF, ...)
}

# hsPrepPdf --------------------------------------------------------------------
hsPrepPdf <- structure(
  function # Prepare writing of PDF file
  ### Opens a PDF device in A4 paper format. After calling this function
  ### all plots go into the specified PDF file in \eqn{strPdf}.
  ### Important: The PDF file needs to be closed explicitely with dev.off() 
  ### after all desired plots have been made.
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
    dev.off()
    
    ## Open PDF file in viewer
    hsShowPdf(pdfFile)
  })

# hsShowPdf --------------------------------------------------------------------
hsShowPdf <- structure(
  function # Open PDF file in PDF viewer
  ### Opens the PDF file of which the full path is given in \emph{Pdf} in a
  ### PDF viewer.
  (
    Pdf,
    ### full path to PDF file
    dbg = TRUE
  ) 
  {
    ##seealso<< \code{\link{hsPrepPdf}}
    
    cmd <- sprintf('"%s" "%s"', getOption("pdfviewer"), Pdf)
    
    if (dbg) {
      cat("cmd:", cmd, "\n")
    }
    
    system(cmd)
  },
  ex = function() {
    
    # Set path to PDF file and open PDF device
    tmpPdf <- tempfile("ex_hsFinishPdf", fileext = ".pdf") 
    hsPrepPdf(tmpPdf)
    
    ## Plot something
    plot(x <- seq(-pi,pi,pi/100), sin(x), type = "l")
    
    ## Finish PDF file.
    dev.off()
    
    ## Open PDF file in viewer.
    hsShowPdf(tmpPdf)
  })
