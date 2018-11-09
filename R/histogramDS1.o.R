#' 
#' @title Computes a histogram of the input variable without plotting.
#' @description This function produces the information required to plot
#' a histogram. This is done without allowing for bins (cells) with number
#' of counts less than the pre-specified disclosure control set for the minimum cell
#' size of a table. If a bin has less counts than this threshold then their counts 
#' and its density are replaced by a 0 value.
#' @param xvect the numeric vector for which the histogram is desired.
#' @param min a numeric, the lower limit of the distribution.
#' @param max a numeric, the upper limit of the distribution.
#' @param num.breaks the number of breaks that the range of the variable is divided.
#' @return a list with an object of class \code{histogram} and the number of invalid cells
#' @export
#' @author Gaye A, Avraam D
#' 
histogramDS1.o <- function (xvect, min, max, num.breaks){

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

  # Check if the number of breaks meets the DataSHIELD privacy criteria (disclosure control for saturation)
  if (num.breaks > (nfilter.glm * length(xvect))){
    studysideMessage <- "FAILED: Number of breaks is too big. It may be disclosive - please shorten"
    return(list(studysideMessage=studysideMessage))
  }else{
    # breaks
    brks <- seq(from=min, to=max, by=(max-min)/num.breaks)
  } 
	
  # Get the histogram object
  histout <- hist(xvect, breaks=brks, plot=FALSE)
  
  # Check if the counts in each 'bin' are more than the disclosure setting for the minimum size of table's cells
  indx <- which(histout$counts > 0 & histout$counts < nfilter.tab)
    
  if(length(indx) > 0){
    # Replace the corresponding counts and densities by zeros
    histout$counts[indx] <- 0
    histout$density[indx] <- 0   
  }
    
  # Return a list with the histogram object and the number of invalid cells
  return(list("histobject"=histout, "invalidcells"=length(indx)))
  
}
# AGGREGATE FUNCTION
# histogramDS1.o