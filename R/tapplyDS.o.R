#' 
#' @title tapplyDS.o called by ds.tapply.o
#' @description An aggregate serverside function that applies a function over a ragged array.
#' @details An aggregate function that uses the native R function tapply() to summarize a variable
#' over one or more factors
#' @param X.name, the name of the variable to be summarized. The user must set the name as a
#' character string in inverted commas
#' @param INDEX.names, the name of a single factor or a list of factors to index the variable 
#' to be summarized. The user must specify this argument in inverted commas.
#' @param FUN.name, the name of a valid function to be applied. The present version of the tapplyDS.o
#' function allows the user to choose one of the four main summarizing functions that are the 'mean',
#' the 'sd', the 'sum', and the 'quantile'.
#' @return an array will be returned to the clientside
#' @author Paul Burton, Demetris Avraam for DataSHIELD Development Team
#' @export
#'
tapplyDS.o <- function(X.name, INDEX.names, FUN.name){

  #########################################################################
  # DataSHIELD MODULE: CAPTURE THE nfilter SETTINGS
  thr <- dsBetaTest::listDisclosureSettingsDS.o()
  nfilter.tab <- as.numeric(thr$nfilter.tab)
  #nfilter.glm <- as.numeric(thr$nfilter.glm)
  nfilter.subset <- as.numeric(thr$nfilter.subset)
  #nfilter.string <- as.numeric(thr$nfilter.string)
  #nfilter.stringShort <- as.numeric(thr$nfilter.stringShort)
  #nfilter.kNN <- as.numeric(thr$nfilter.kNN)
  #datashield.privacyLevel <- as.numeric(thr$datashield.privacyLevel)
  #########################################################################

  if(is.character(X.name)){
	  X<-eval(parse(text=X.name))
	}else{
    studysideMessage<-"ERROR: X.name must be specified as a character string"
    return(list(studysideMessage=studysideMessage))
  }

  if(is.character(INDEX.names)){
  	INDEX<-eval(parse(text=INDEX.names))
  }else{
    studysideMessage<-"ERROR: INDEX.names must be specified as follows: 'V1' or 'list(V1,V2,V3)' "
    return(list(studysideMessage=studysideMessage))
  }

  #################
  #Valid functions# 
  #################

  ###MEAN
  if(FUN.name=="mean" || FUN.name=="Mean" || FUN.name=="MEAN"){
    Mean <- tapply(X[complete.cases(X)], INDEX[complete.cases(X)], mean)
    N <- tapply(X[complete.cases(X)], INDEX[complete.cases(X)], length)
    if(min(N)<nfilter.tab){
      studysideMessage <- "ERROR: at least one group defined by INDEX is < nfilter.tab. The output cannot therefore be returned to the clientside. But the function ds.tapply.assign.o may still be used to write the output to the data servers with no clientside return"
      return(studysideMessage)
    }
    output <- data.frame(Mean, N)
  }

  ###SD
  if(FUN.name=="sd" || FUN.name=="SD"){
    SD <- tapply(X[complete.cases(X)], INDEX[complete.cases(X)], sd)
    N <- tapply(X[complete.cases(X)], INDEX[complete.cases(X)], length)
    if(min(N)<nfilter.tab){
      studysideMessage <- "ERROR: at least one group defined by INDEX is < nfilter.tab. The output cannot therefore be returned to the clientside. But the function ds.tapply.assign.o may still be used to write the output to the data servers with no clientside return"
      return(studysideMessage)
    }
    output <- data.frame(SD, N)
  }

  ###SUM
  if(FUN.name=="sum" || FUN.name=="Sum" || FUN.name=="SUM"){
    Sum <- tapply(X[complete.cases(X)], INDEX[complete.cases(X)], sum)
    N <- tapply(X[complete.cases(X)], INDEX[complete.cases(X)], length)
    if(min(N)<nfilter.tab){
      studysideMessage <- "ERROR: at least one group defined by INDEX is < nfilter.tab. The output cannot therefore be returned to the clientside. But the function ds.tapply.assign.o may still be used to write the output to the data servers with no clientside return"
      return(studysideMessage)
    }
    output <- data.frame(Sum, N)
  }
 
  ###QUANTILE
  if(FUN.name=="quantile" || FUN.name=="Quantile" || FUN.name=="QUANTILE"){
    probs.vector <- c(0.05,0.1,0.2,0.25,0.3,0.33,0.4,0.5,0.6,0.67,0.7,0.75,0.8,0.9,0.95)
    quantile <- tapply(X[complete.cases(X)], INDEX[complete.cases(X)], quantile, probs=probs.vector)
    N <- tapply(X[complete.cases(X)], INDEX[complete.cases(X)], length)
    if(min(N)<nfilter.tab){
      studysideMessage <- "ERROR: at least one group defined by INDEX is < nfilter.tab. The output cannot therefore be returned to the clientside. But the function ds.tapply.assign.o may still be used to write the output to the data servers with no clientside return"
      return(studysideMessage)
    }
    output <- data.frame(quantile, N)
  }

  return(output)

}
#AGGREGATE FUNCTION
# tapplyDS.o 
 
