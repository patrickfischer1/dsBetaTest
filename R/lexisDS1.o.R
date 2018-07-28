function(exitCol=NULL){

#############################################################
#MODULE 1: CAPTURE THE nfilter SETTINGS                     #
thr<-.AGGREGATE$listDisclosureSettingsDS.o()				#
#nfilter.tab<-as.numeric(thr$nfilter.tab)					#
#nfilter.glm<-as.numeric(thr$nfilter.glm)					#
#nfilter.subset<-as.numeric(thr$nfilter.subset)         	#
nfilter.string<-as.numeric(thr$nfilter.string)              #
#############################################################



exitCol.length<-length(strsplit(exitCol,"")[[1]])
if(exitCol.length>nfilter.string){
 errorMessage<-"ERROR: character string naming exitCol is too long please shorten name"
 out.obj<-list(errorMessage=errorMessage)
 return(out.obj)
}


exposure<-eval(parse(text=exitCol))


max.time<-max(exposure,na.rm=TRUE)
random.multiplier<-runif(1,1.01,1.05)

max.time<-max.time*random.multiplier

out.obj<-list(max.time=max.time)

return(out.obj)

}
#AGGREGATE FUNCTION
# lexisDS1.o
