#' 
#' @title Calculates the coordinates of the centroid of each n nearest neighbours
#' @description This function calculates the centroids for each n nearest neighbours.
#' @details The function searches for the n-1 nearest neighbours of each data point of the original
#' dataset. The nearest neighbours are the points with the minimum Euclidean distances from the 
#' point of interest. The centroid of the point of interest and its n-1 nearest neighbours, is 
#' calculated. The coordinates of centroids return to the client side function in order to plot 
#' non-disclusive scatter plots.    
#' @param x the name of a numeric vector, the x-variable.
#' @param y the name of a numeric vector, the y-variable.
#' @param n the number of the nearest neghbours for which their centroid is calculated.   
#' @return a list with the x and y coordinates of the centroids
#' @author Avraam, D.
#' @export
#' 
ScatterPlotDS <- function(x, y, n){

  # Load the RANN package to use the 'nn2' function that searches for the Nearest Neighbours  
  library(RANN)

  # Cbind the columns of the two variables and remove any rows that include NAs
  data.table <- cbind.data.frame(x, y)
  data.complete <- na.omit(data.table)
		
  x <- as.vector(data.complete[,1])
  y <- as.vector(data.complete[,2])
  
  # Create a data.frame for the variables
  data <- data.frame(x, y)
  
  # Calculate the length of the data.frame after ommitting any rows with NAs 
  N.data <- dim(data)[1]

  # Check if n is an integer and greater than or equal to 3
  if(n < 3){   
    stop("n must be greater than or equal to 3.", call.=FALSE)
  }else{
    neighbours = n
  }

  # Find the two nearest neighbours of each data point 
  nearest <- nn2(data, k = neighbours)

  # Calculate the centroid of each n nearest data points 
  x.centroid <- matrix()
  y.centroid <- matrix()
  for (i in 1:N.data){
    x.centroid[i] <- mean(x[nearest$nn.idx[i,1:neighbours]])
    y.centroid[i] <- mean(y[nearest$nn.idx[i,1:neighbours]])
  }

  # Return a list with the x and y coordinates of the centroids
  return(list(x.centroid, y.centroid))

}
