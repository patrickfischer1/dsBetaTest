#' @title
#' lexisDS1
#'
#' @description
#' The first serverside function called by ds.lexis. This is an aggregate function which identfies
#' the maximum surival time in each study. Adds a random increment to that maximum (to protect against
#' disclosure) and then passes these maxima back to ds.lexis. ds.lexis then identifies the maximum of the
#' maxima and this allows a coherent common set of time intervals to be set across all studies when
#' the collapsed dataframe is expanded by lexisDS2, usually in preparing for a piecewise exponential
#' regression. For more details see the extensive header for ds.lexis.
#'
#' @export
#'
lexisDS1.b<-function(exitCol=NULL){

	if(is.character(exitCol)){
		exposure<-eval(parse(text=exitCol))
	}else{
		exposure<-exitCol
	}

max.time<-max(exposure,na.rm=TRUE)
random.multiplier<-runif(1,1.01,1.05)

max.time<-max.time*random.multiplier

out.obj<-list(max.time=max.time)

return(out.obj)

}
#MUST BE AGGREGATE FUNCTION!!
#lexisDS1.b
