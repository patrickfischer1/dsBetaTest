#' 
#' @title returns the minimum and maximum of a numeric vector
#' @description this function is similar to R function \code{range} but instead to not return 
#' the real minimum and maximum, the computed values are multiplied by a very small random number. 
#' @param xvect a numerical 
#' @return  a numeric vector which contains the minimum and the maximum
#' @author Gaye A, Avraam D
#' @export
#'
rangeDS.o <- function(xvect){
  
  # Print an error message if the input vector is not a numeric
  if(!(is.numeric(xvect))){
    output <- "The input vector is not a numeric!"
  }else{
    rr <- c(min(xvect, na.rm=TRUE), max(xvect, na.rm=TRUE))
    if(rr[1] < 0){ r.min <- rr[1] * runif(1, 1.01, 1.05) }else{ r.min <- rr[1] * runif(1, 0.95, 0.99) }
    if(rr[2] < 0){ r.max <- rr[2] * runif(1, 0.95, 0.99) }else{ r.max <- rr[2] * runif(1, 1.01, 1.05) }
    output <- c(r.min, r.max)
  }
  
  return(output)

}
# AGGREGATE FUNCTION
# rangeDS.o