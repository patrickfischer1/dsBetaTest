#' 
#' @title recodeValuesDS1
#' @description The first serverside function called by ds.recodeValues.o This is an
#' aggregate function.
#' @details For more details see the extensive header for ds.recodeValues.o
#' @param var.name.text a clientside specified character string identifying the
#' variable to be recoded
#' @param values2replace.text a clientside specified character string identifying the
#' original values to be changed
#' @param new.values.text a clientside specified character string identifying the new
#' values to be allocated
#' the values identified by values2replace.text and new.values.text are of the same
#' length and in the same order
#' @author Burton PR
#' @export
recodeValuesDS1.o <- function(var.name.text=NULL, values2replace.text=NULL, new.values.text=NULL){
  
  
  #############################################################
  # MODULE 1: CAPTURE THE nfilter SETTINGS                    #
  thr<-.AGGREGATE$listDisclosureSettingsDS.o()				#
  #nfilter.tab<-as.numeric(thr$nfilter.tab)					#
  #nfilter.glm<-as.numeric(thr$nfilter.glm)					#
  nfilter.subset<-as.numeric(thr$nfilter.subset)          	#
  nfilter.string<-as.numeric(thr$nfilter.string)              #
  nfilter.stringShort<-as.numeric(thr$nfilter.stringShort)    #
  nfilter.kNN<-as.numeric(thr$nfilter.kNN)                    #
  #############################################################
  
  
  var.name.text.c<-unlist(strsplit(var.name.text, split=","))
  var2recode<-eval(parse(text=var.name.text.c))
  
  
  values2replace.c <- unlist(strsplit(values2replace.text, split=","))
  values2replace <- as.numeric(values2replace.c)
  
  new.values.c <- unlist(strsplit(new.values.text, split=","))
  new.values <- as.numeric(new.values.c)
  
  
  var.recoded <- var2recode
  
  for(j in 1:length(var2recode)){
    for(k in 1:length(values2replace)){
      if(is.na(var2recode[j])||is.na(values2replace[k])){
        if(is.na(var2recode[j])&&is.na(values2replace[k])){
          var.recoded[j]<-new.values[k]
        }
      }
      else
      {
        if(var2recode[j]==values2replace[k]){
          var.recoded[j]<-new.values[k]
        }
      }
    }
  }
  
  return.obj <- list(var.recoded=var.recoded)
  
  # DISCLOSURE TRAP ON LENGTH OF NA AND non-NA ELEMENTS OF ORIGINAL AND RECODED VECTORS
  mark.original <- complete.cases(var2recode)
  non.NA.original.vector <- var2recode[mark.original]
  non.NA.length.original <- length(non.NA.original.vector)
  
  mark.recoded <- complete.cases(var.recoded)
  non.NA.recoded.vector <- var.recoded[mark.recoded]
  non.NA.length.recoded <- length(non.NA.recoded.vector)
  
  difference.non.NA.lengths <- abs(non.NA.length.recoded-non.NA.length.original)
  
  
  
  # Non-NA SUBSET OF RECODED VARIABLE SMALLER THAN MINIMUM SUBSET SIZE - BLOCK CREATION OF RECODED VECTOR
  # AND RETURN MESSAGE
  if(non.NA.length.recoded<nfilter.subset){
    studysideMessage<-"Error: number of non-NA elements of recoded vector < minimum subset size"
    return(list(studysideMessage=studysideMessage))
  }
  
  
  ########################################################################
  ######### MODULE WARNING OF POTENTIAL DIFFERENCE ATTACK ################
  ########################################################################
  
  if((difference.non.NA.lengths<nfilter.subset)&&(difference.non.NA.lengths>0)){
    studysideWarning1<-"Warning: DataSHIELD monitors every session for potentially disclosive analytic requests."
    studysideWarning2<-"The analysis you just submitted has generated a recoded variable in which the number of non-missing"
    studysideWarning3<-"elements differs - but only very slightly - from the original variable. This is most likely to be"
    studysideWarning4<-"an innocent consequence of your recoding needs. However, it could in theory be one step"
    studysideWarning5<-"in a difference-based attack aimed at identifying individuals. This analytic request has"
    studysideWarning6<-"therefore been highlighted in the session log file. Please be reassured, if you do not try"
    studysideWarning7<-"to identify individuals this will cause you no difficulty. However, if you do plan a "
    studysideWarning8<-"malicious attempt to identify individuals by differencing, this will become obvious in the"
    studysideWarning9<-"session log and you will be sanctioned. Possible consequences include loss of future access"
    studysideWarning10<-"to DataSHIELD and/or legal penalties."
    
    return.message<-list(studysideWarning1,studysideWarning2,studysideWarning3,studysideWarning4,
                         studysideWarning5,studysideWarning6,studysideWarning7,studysideWarning8,
                         studysideWarning9,studysideWarning10)
  }else{
    return.message<-"Recoding undertaken without problems"
  }
  
  ########################################################################
  ######### MODULE WARNING OF POTENTIAL DIFFERENCE ATTACK ################
  ########################################################################
  
  return(return.message)
  
}  
#AGGREGATE FUNCTION
# recodeValuesDS1.o