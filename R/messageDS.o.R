function(message.object.name) 
{
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

#TEST IF message.object.name EXISTS AS AN OBJECT ON SERVERSIDE
#message.object.name.text<-paste0(message.object.name)
#boole.A1.exists<-exists(message.object.name.text)


A1.exists.text<-paste0("exists('",message.object.name,"')")

boole.A1.exists<-eval(parse(text=A1.exists.text))




if(!boole.A1.exists) {
out.obj<-"Error: the object <message.obj> does not exist in this datasource" 
}



#IF message.object.name EXISTS, CHECK WHETHER IT CURRENTLY CONTAINS A studysideMessage

if(boole.A1.exists){
message.object.name.object<-eval(parse(text=message.object.name))

if(class(message.object.name.object)=="list"){
	s.message.available<-FALSE
	for(j in length(names(message.object.name.object))){
	if(names(message.object.name.object)[j]=="studysideMessage"){
		s.message.available<-TRUE
		}
	}
	
	if(s.message.available){
	out.obj<-message.object.name.object$studysideMessage
	}

	}else{
	no.s.message<-"ALL OK: there are no studysideMessage(s) on this datasource"
	out.obj<-no.s.message
	}

  }
  
  
#CHECK NO NUMERICS IN MESSAGE (TO STOP DATA BEING SEEN)
char.list<-unlist(strsplit(out.obj,split=""))
list.length<-length(char.list)

trap.numeric<-0


if(list.length>nfilter.string)trap.numeric<-1

for(j in 1:list.length)
{
   for(k in c("0","1","2","3","4","5","6","7","8","9"))
   {
    if(char.list[j]==k)trap.numeric<-1
   }
}

if(trap.numeric>0){
   out.obj<-"Error: studysideMessage(s) cannot include numbers and must <= eighty chars"
   }
   
return(MESSAGE=out.obj)
}
#AGGREGATE FUNCTION
# messageDS.o


