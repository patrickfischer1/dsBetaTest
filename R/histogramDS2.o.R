#' 
#' @title Computes a histogram of the input variable without plotting.
#' @description This functions produces the information required to plot
#' a histogram. This is done by locating the centroids for each k nearest neighbours
#' of the input data and then producing information for the histogram of the centroids. 
#' @param xvect the numeric vector for which the histogram is desired
#' @param min a numeric, the lower limit of the distribution
#' @param max a numeric, the upper limit of the distribution
#' @param num.breaks the number of breaks that the range of the variable is divided
#' @param num.neighbours the number of the nearest neghbours for which their centroid is calculated
#' @return a list with an object of class \code{histogram} and the number of invalid cells. The
#' number of invalid cells for the method "Centroids" is set to null.
#' @export
#' @author Avraam D
#' 
histogramDS2.o <- function (xvect, min, max, num.breaks, num.neighbours){

  #############################################################
  # MODULE 1: CAPTURE THE nfilter SETTINGS                    
  thr <- .AGGREGATE$listDisclosureSettingsDS.o()				
  nfilter.tab <- as.numeric(thr$nfilter.tab)					
  nfilter.glm <- as.numeric(thr$nfilter.glm)					
  nfilter.subset <- as.numeric(thr$nfilter.subset)          	
  nfilter.string <- as.numeric(thr$nfilter.string)            
  nfilter.stringShort <- as.numeric(thr$nfilter.stringShort)  
  nfilter.kNN <- as.numeric(thr$nfilter.kNN)                  
  #############################################################

  # Check if the number of breaks meets the DataSHIELD privacy criteria
  if (num.breaks > (nfilter.glm * length(xvect))){
    studysideMessage <- "FAILED: Number of breaks is too big. It may be disclosive - please shorten"
    return(list(studysideMessage=studysideMessage))
  }else{
    # breaks
    brks <- seq(from=min, to=max, by=(max-min)/num.breaks)
  }
  
  # Check if num.neighbours is integer and has a value greater than or equal to the nfilter.kNN 
  # and less than or equal to the length of the variable minus the nfilter.kNN
  n <- num.neighbours
  N.data <- length(na.omit(xvect))  
  if(n < nfilter.kNN | n > (N.data - nfilter.kNN)){   
    stop(paste0("n must be greater than or equal to ", nfilter.kNN, "and less than or equal to ", (N.data-nfilter.kNN), "."), call.=FALSE)
  }else{
    neighbours = n
  }
  
  x.centroids <- .AGGREGATE$centroids1dDS.o(xvect, neighbours)  
  xvect <- x.centroids
  
  # Get the histogram object
  histout <- hist(xvect, breaks=brks, plot=FALSE)
  
  # Return a list with the histogram object and the number of invalid cells
  return(list("histobject"=histout, "invalidcells"=0))
  
}
# AGGREGATE FUNCTION
# histogramDS2.o