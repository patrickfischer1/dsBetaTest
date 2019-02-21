#' 
#' @title reShapeDS.o called by ds.reShape.o
#' @description An assign serverside function that reshapes a given data frame between wide and long formats.
#' @details This function reshapes a data frame between 'wide' format with repeated
#' measurements in separate columns of the same record and 'long' format with the repeated
#' measurements in separate records and assign it at the servers. 
#' @param data.name, the name of the data frame.
#' @param varying.transmit, names of sets of variables in the wide format that correspond to single
#' variables in long format ('time-varying').
#' @param v.names.transmit, the names of variables in the long format that correspond to multiple variables
#' in the wide format
#' @param timevar.name, the name of the variable in long format that differentiates multiple records from the same
#' group or individual. If more than one record matches, the first will be taken
#' @param idvar.name, Names of one or more variables in long format that identify multiple records from
#' the same group/individual. These variables may also be present in wide format.
#' @param drop.transmit,	a vector of names of variables to drop before reshaping.
#' @param direction, a character string, matched to either "wide" to reshape to wide 
#' format, or "long" to reshape to long format.
#' @param sep, a character vector of length 1, indicating a separating character in the variable
#' names in the wide format. This is used for guessing v.names and times arguments based on the
#' names in varying. This is also used to create variable names when reshaping to wide format.
#' @return a dataframe will be created on the serverside and named according to the \code{newobj} argument of the
#' clientside function ds.reShape.o()
#' @author Demetris Avraam, Paul Burton for DataSHIELD Development Team 
#' @export
#'
reShapeDS.o <- function(data.name, varying.transmit, v.names.transmit, timevar.name, idvar.name, drop.transmit, direction, sep){
  
  datatext <- paste0("data.frame(",data.name,")")
  data <- eval(parse(text=datatext))
  
  timevar <- timevar.name
  idvar <- idvar.name
  direction <- direction
  sep <- sep
  
  if(!is.null(varying.transmit)){
    varying<-unlist(strsplit(varying.transmit, split=","))
  }else{
    varying<-NULL
  }
  
  if(!is.null(v.names.transmit)){
    v.names<-unlist(strsplit(v.names.transmit, split=","))
  }else{
    v.names<-NULL
  }
  
  if(!is.null(drop.transmit)){
    drop<-unlist(strsplit(drop.transmit, split=","))
  }else{
    drop<-NULL
  }
  
  split = if (sep == "") {
    list(regexp = "[A-Za-z][0-9]", include = TRUE)
  } else {
    list(regexp = sep, include = FALSE, fixed = TRUE)
  }
 
 if(direction=="wide"){
   output <- reshape(data=data, varying=varying, v.names=v.names, timevar=timevar, idvar=idvar, 
                     drop=drop, direction=direction, new.row.names = NULL, sep=sep, split=split) 
 }
 
 if(direction=="long"){
    output <- reshape(data=data, varying=varying, timevar=timevar, idvar=idvar, direction=direction)
 }
 
  return(output)

}
#Assign function
# reShapeDS.o


