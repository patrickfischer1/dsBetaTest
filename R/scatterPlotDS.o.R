function(x, y, n){

#############################################################
# MODULE 1: CAPTURE THE nfilter SETTINGS                    #
thr <- .AGGREGATE$listDisclosureSettingsDS.o()				#
nfilter.tab <- as.numeric(thr$nfilter.tab)					#
nfilter.glm <- as.numeric(thr$nfilter.glm)					#
nfilter.subset <- as.numeric(thr$nfilter.subset)          	#
nfilter.string <- as.numeric(thr$nfilter.string)            #
nfilter.stringShort <- as.numeric(thr$nfilter.stringShort)  #
nfilter.kNN <- as.numeric(thr$nfilter.kNN)                  #
#############################################################



  # Load the RANN package to use the 'nn2' function that searches for the Nearest Neighbours  
  library(RANN)

  # Cbind the columns of the two variables and remove any rows that include NAs
  data.table <- cbind.data.frame(x, y)
  data.complete <- na.omit(data.table)
		
  x <- as.vector(data.complete[,1])
  y <- as.vector(data.complete[,2])
  
  # standardise the variables
  x.standardised <- (x-mean(x))/sd(x)
  y.standardised <- (y-mean(y))/sd(y)

  # Create a data.frame for the variables
  data <- data.frame(x.standardised, y.standardised)

  # Calculate the length of the data.frame after ommitting any rows with NAs 
  N.data <- dim(data)[1]
 

  # Check if n is integer and has a value greater than or equal to the pre-specified threshold 
  # and less than or equal to the length of rows of data.complete minus the pre-specified threshold
  if(n < nfilter.kNN | n > (N.data - nfilter.kNN)){   
    stop(paste0("n must be greater than or equal to ", nfilter.kNN, "and less than or equal to ", (N.data-nfilter.kNN), "."), call.=FALSE)
  }else{
    neighbours = n
  }



  # Find the n-1 nearest neighbours of each data point 
  nearest <- nn2(data, k = neighbours)




  
  # Calculate the centroid of each n nearest data points 
  x.centroid <- matrix()
  y.centroid <- matrix()
  for (i in 1:N.data){
    x.centroid[i] <- mean(x.standardised[nearest$nn.idx[i,1:neighbours]])
    y.centroid[i] <- mean(y.standardised[nearest$nn.idx[i,1:neighbours]])
  }

  # Calculate the scaling factor
  x.scalingFactor <- sd(x.standardised)/sd(x.centroid)
  y.scalingFactor <- sd(y.standardised)/sd(y.centroid)

  # Apply the scaling factor to the centroids
  x.masked <- x.centroid * x.scalingFactor
  y.masked <- y.centroid * y.scalingFactor

  # Shift the centroids back to the actual position and scale of the original data
  x.new <- (x.masked * sd(x)) + mean(x)
  y.new <- (y.masked * sd(y)) + mean(y)
  
  # Return a list with the x and y coordinates of the centroids
  return(list(x.new, y.new))

}
# AGGREGATE FUNCTION
# scatterPlotDS.o

