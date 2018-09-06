#'
#' @title testObjExistsDS
#'
#' @description
#' This is aggregate function.
#' @details For more details see the extensive header for ds.testObjExists
#'
#' @export
#'
testObjExistsDS.o <- function(test.obj.name=NULL){

  test.obj.exists<-FALSE
  test.obj.class<-NULL
  
  #Restrict tests to settings where name does exist to avoid error termination
  
  if(exists(test.obj.name)){
  test.obj.exists<-TRUE
  test.obj<-eval(parse(text=test.obj.name))
  test.obj.class<-class(test.obj)
  }

return(list(test.obj.exists=test.obj.exists,test.obj.class=test.obj.class))

}
#AGGREGATE FUNCTION
#testObjExistsDS.o




