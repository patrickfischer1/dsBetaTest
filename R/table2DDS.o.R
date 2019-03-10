#'
#' @title Computes statistical mean of a vectores
#' @description Some text.
#' @details Some text.
#' @param xvect a vector
#' @param yvect a vector
#' @return output table and message
#' @author Avraam D., Burton PR.
#' @export
#'

table2DDS.o <- function(xvect,yvect){

#############################################################
#MODULE 1: CAPTURE THE nfilter SETTINGS                     #
#OLD DEV PLATFORM thr<-.AGGREGATE$listDisclosureSettingsDS.o()				
#WHEN IMPORTED FROM GitHub thr<-listDisclosureSettingsDS.o()#
thr<-dsBetaTest::listDisclosureSettingsDS.o()				#
nfilter.tab<-as.numeric(thr$nfilter.tab)					#
#nfilter.glm<-as.numeric(thr$nfilter.glm)					#
#nfilter.subset<-as.numeric(thr$nfilter.subset)             #
#nfilter.string<-as.numeric(thr$nfilter.string)             #
#nfilter.stringShort<-as.numeric(thr$nfilter.stringShort)   #
#nfilter.kNN<-as.numeric(thr$nfilter.kNN)                   #
#############################################################

  # tabulate the input vector and output the result in a data frame format
  aa <- table(xvect, yvect)
  bb <- matrix(NA, nrow=dim(aa)[1], ncol=dim(aa)[2])
  for(i in 1:dim(aa)[1]){
    bb[i,] <- aa[i,]
  }
  bb <- rbind(bb, colSums(bb))
  bb <- cbind(bb, rowSums(bb))
  cc <- as.data.frame(bb)
  colnames(cc) <- c(levels(as.factor(yvect)), "Total")
  rownames(cc) <- c(levels(as.factor(xvect)), "Total")

  # the minimum non-zero number of observations that is allowed in a single cell
  nfilter <- nfilter.tab
  
  # check for invalid cells if any found change them to 'NA' and set the validity message accordingly
  validity <- "valid Table"
  for(i in 1: dim(cc)[2]){
    indx <- which(cc[1:(dim(cc)[1] - 1),i] > 0 & cc[1:(dim(cc)[1] - 1),i] < nfilter)
    if(length(indx) > 0){
      cc[1:(dim(cc)[1] - 1), 1:(dim(cc)[2] - 1)] <- NA
      validity <- "invalid table - invalid counts present"
      break
    }    
  }

  # return output table and message
  return(list(table=cc, message=validity))
}
#AGGREGATE
# table2DDS.o

