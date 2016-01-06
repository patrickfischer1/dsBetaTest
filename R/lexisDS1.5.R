#' @title
#' lexisDS1.5.R
#'
#' @description
#' TODO
#'
#' @export
#'

lexisDS1.5 <- function(exitCol=NULL){

	if(is.character(exitCol)){
		exposure<-eval(parse(text=exitCol))
	}else{
		exposure<-exitCol
	}

max.time<-max(exposure,na.rm=TRUE)
random.multiplier<-runif(1,1.05,1.25)

max.time<-max.time*random.multiplier

out.obj<-list(max.time=max.time)

return(out.obj)

}
#MUST BE AGGREGATE FUNCTION!!
#lexisDS1.5
