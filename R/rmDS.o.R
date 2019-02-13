function(x.name.transmit)			 			 
{
#########################################################################
# DataSHIELD MODULE: CAPTURE THE nfilter SETTINGS           			#
thr<-.AGGREGATE$listDisclosureSettingsDS.o()							#
#nfilter.tab<-as.numeric(thr$nfilter.tab)								#
#nfilter.glm<-as.numeric(thr$nfilter.glm)								#
#nfilter.subset<-as.numeric(thr$nfilter.subset)          				#
#nfilter.string<-as.numeric(thr$nfilter.string)              			#
nfilter.stringShort<-as.numeric(thr$nfilter.stringShort)    			#
#nfilter.kNN<-as.numeric(thr$nfilter.kNN)								#
#datashield.privacyLevel<-as.numeric(thr$datashield.privacyLevel)        #
#########################################################################

#manage x.name


	#check text to be activated is not too long because of disclosure risk
x.name.numchars<-length(unlist(strsplit(x.name.transmit,split="")))

if(x.name.numchars>nfilter.stringShort){
   return.message<-
   paste0("Disclosure risk, number of characters in x.name must not exceed nfilter.stringShort which is currently set at: ",nfilter.stringShort)
    return(list(return.message=return.message))
}

#convert x.name format from transmittable to actionable form (a vector of character strings)
	x.name<-unlist(strsplit(x.name.transmit, split=","))

	#tests whether already exists
test.already.exists<-exists(x.name)

if(!test.already.exists){
   return.message<-
   paste0("Object to be deleted, i.e. ",x.name, " , does not exist so does not need deleting")
    return(list(return.message=return.message))
}

rm(list=c(x.name),pos=1)

#test whether still exists
test.still.exists<-exists(x.name)

if(test.still.exists){
   return.message<-paste0("Object to be deleted, i.e. ",x.name, " , still exists")
	    return(list(return.message=return.message))

	}else{
		return.message<-paste0("Object successfully deleted")
	    return(list(return.message=return.message))
	}


return(return.message=return.message,x.name=x.name)
}
#AGGREGATE FUNCTION
# rmDS.o

