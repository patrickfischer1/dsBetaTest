#' 
#' @title listDisclosureSettingsDS
#' @description This serverside function is an aggregate function that is called by the
#' ds.listDisclosureSettings
#' @details For more details see the extensive header for ds.listDisclosureSettings
#' @author Paul Burton
#' @export
#'
listDisclosureSettingsDS.o <- function(){

  nf.tab <- getOption("nfilter.tab")
  nf.subset <- getOption("nfilter.subset")
  nf.glm <- getOption("nfilter.glm")
  nf.string <- getOption("nfilter.string")
  nf.stringShort <- getOption("nfilter.stringShort")
  nf.kNN <- getOption("nfilter.kNN")	
  nfilter.privacy.old <- getOption("datashield.privacyLevel")
	
  return(list(nfilter.tab=nf.tab,nfilter.subset=nf.subset,nfilter.glm=nf.glm,nfilter.string=nf.string,
	            nfilter.stringShort=nf.stringShort,nfilter.kNN=nf.kNN,nfilter.privacy.old=nfilter.privacy.old))
				
}
#AGGREGATE FUNCTION
# listDisclosureSettingsDS.o
