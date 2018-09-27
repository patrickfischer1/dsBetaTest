#' 
#' @title recodeValuesDS2
#' @description The second serverside function called by ds.recodeValues.o This is an
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
#' @param numeric.output.format.possible a clientside generated Boolean variable
#' indicating whether the vector of new values contains any non-numerics
#' which will make a solely numeric output impossible
#' @param force.output.format a clientside provided character string determining
#' whether to force a numeric output. If so, and if the
#' the vector of new values contains non-numerics they will be set to NaN
#' @param v2r.numeric a clientside provided Boolean which if TRUE forces all input
#' values (values2replace) to be numeric. If so, and if the
#' the vector of values2replace contains non-numerics they will be set to NaN
#' @author Burton PR
#' @export
recodeValuesDS2.o <- function(var.name.text=NULL, values2replace.text=NULL, new.values.text=NULL,numeric.output.format.possible,force.output.format="no",v2r.numeric=NULL){
  
  #############################################################
  #MODULE 1: CAPTURE THE nfilter SETTINGS                     #
  thr<-.AGGREGATE$listDisclosureSettingsDS.o()				#
  #nfilter.tab<-as.numeric(thr$nfilter.tab)					#
  #nfilter.glm<-as.numeric(thr$nfilter.glm)					#
  nfilter.subset<-as.numeric(thr$nfilter.subset)          	#
  nfilter.string<-as.numeric(thr$nfilter.string)              #
  nfilter.stringShort<-as.numeric(thr$nfilter.stringShort)    #
  nfilter.kNN<-as.numeric(thr$nfilter.kNN)                    #
  #############################################################
  
  
  #DISCLOSURE TRAPS
  var.name.text.chars<-strsplit(var.name.text,split="")
  if(length(var.name.text.chars[[1]])>nfilter.stringShort){
    studysideMessage<-"Error: var.name.text argument too long (see nfilter.stringShort)"
    return(list(studysideMessage=studysideMessage))
  }
  
  values2replace.text.chars<-strsplit(values2replace.text,split="")
  if(length(values2replace.text.chars[[1]])>nfilter.stringShort){
    studysideMessage<-"Error: values2replace.text argument too long (see nfilter.stringShort)"
    return(list(studysideMessage=studysideMessage))
  }
  
  new.values.text.chars<-strsplit(new.values.text,split="")
  if(length(new.values.text.chars[[1]])>nfilter.stringShort){
    studysideMessage<-"Error: new.values.text argument too long(see nfilter.stringShort)"
    return(list(studysideMessage=studysideMessage))
  }
  
  
  var.name.text.c<-unlist(strsplit(var.name.text, split=","))
  var2recode<-eval(parse(text=var.name.text.c))
  
  
  values2replace.c<-unlist(strsplit(values2replace.text, split=","))
  
  if(v2r.numeric){
    values2replace<-as.numeric(values2replace.c)
  }else{
    values2replace<-values2replace.c
  }
  
  
  new.values.c<-unlist(strsplit(new.values.text, split=","))
  
  #SHOULD OUTPUT FORMAT BE NUMERIC OR CHARACTER
  
  new.values<-new.values.c
  
  numeric.output.format.still.possible<-(is.numeric(var2recode)&&numeric.output.format.possible)
  
  if(numeric.output.format.still.possible || force.output.format=="numeric"){
    new.values<-as.numeric(new.values.c)
  }
  
  if(force.output.format=="character"){
    new.values<-new.values.c
  }
  
  
  var.recoded<-var2recode
  
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
  
  return.obj<-var.recoded
  
  #DISCLOSURE TRAP ON LENGTH OF NA AND non-NA ELEMENTS OF ORIGINAL AND RECODED VECTORS
  mark.original<-complete.cases(var2recode)
  non.NA.original.vector<-var2recode[mark.original]
  non.NA.length.original<-length(non.NA.original.vector)
  
  mark.recoded<-complete.cases(var.recoded)
  non.NA.recoded.vector<-var.recoded[mark.recoded]
  non.NA.length.recoded<-length(non.NA.recoded.vector)
  
  difference.non.NA.lengths<-abs(non.NA.length.recoded-non.NA.length.original)
  
  
  #Non-NA SUBSET OF RECODED VARIABLE SMALLER THAN MINIMUM SUBSET SIZE - BLOCK CREATION OF RECODED VECTOR
  #AND RETURN MESSAGE
  if(non.NA.length.recoded<nfilter.subset){
    studysideMessage<-"Error: number of non-NA elements of recoded vector < minimum subset size"
    return(list(studysideMessage=studysideMessage))
  }
  
  
  return(return.obj)
  
}  
#ASSIGN FUNCTION
# recodeValuesDS2.o