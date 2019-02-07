#' 
#' @title seqDS.o called by ds.seq.o
#' @description An assign serverside function that generates sequence vectors.
#' @details An assign function that uses the native R function seq() to create
#' any one of a flexible range of sequence vectors that can then be used to help
#' manage and analyse data.
#' @param FROM.value.char, a number given as a character denoting the starting value of the sequence. 
#' The default value is set to 1.
#' @param BY.value.char, a number given as a character indicating the increment of the sequence.
#' The default value is set to 1.
#' @param LENGTH.OUT.value.char, a non-negative number given as character denoting the desired 
#' length of the sequence.
#' @param ALONG.WITH.name, is the name of a serverside vector in inverted commas that is used to
#' determine the length of the created vector. 
#' @return a sequence vector will be created on the serverside and named according to the
#' \code{newobj} argument of the clientside function ds.seq.o()
#' @author Paul Burton, Demetris Avraam for DataSHIELD Development Team
#' @export
#'
seqDS.o <- function(FROM.value.char, BY.value.char, LENGTH.OUT.value.char, ALONG.WITH.name){
 
  if(is.character(FROM.value.char) && is.numeric(eval(parse(text=FROM.value.char)))){
  	FROM <- eval(parse(text=FROM.value.char))
	}else{
    studysideMessage <- "ERROR: FROM.value.char must be specified as a real number in inverted commas eg '-3.74' or '0'"
    return(list(studysideMessage=studysideMessage))
  }

  if(is.character(BY.value.char)&&is.numeric(eval(parse(text=BY.value.char)))){
  	BY <- eval(parse(text=BY.value.char))
	}else{
    studysideMessage <- "ERROR: BY.value.char must be specified as a real number in inverted commas eg '-3.74' or '0'"
    return(list(studysideMessage=studysideMessage))
  }
 
  if(!is.null(LENGTH.OUT.value.char)){
		if(is.character(LENGTH.OUT.value.char) && is.numeric(eval(parse(text=LENGTH.OUT.value.char)))){
	  	LENGTH.OUT <- eval(parse(text=LENGTH.OUT.value.char))
		}else{
		  studysideMessage <- "ERROR: If LENGTH.OUT.value.char is non-NULL, it must specify a positive integer in inverted commas eg '14'" 
		  return(list(studysideMessage=studysideMessage))
		}
	}

  if(is.null(LENGTH.OUT.value.char)){
    LENGTH.OUT <- NULL
  }

  if(!is.null(ALONG.WITH.name)){
		if(is.character(ALONG.WITH.name)){
	  	ALONG.WITH <- eval(parse(text=ALONG.WITH.name))
		}else{
		  studysideMessage <- "ERROR: If ALONG.WITH.name is non-NULL, it must specify the name of a serverside vector in inverted commas" 
		  return(list(studysideMessage=studysideMessage))
		}
	}
 
  if(is.null(ALONG.WITH.name)){
    ALONG.WITH <- NULL
  }
 
  #the <to> argument to seq must be missed out altogether. The output length will then by default
  #be determined by the length of the vector specified by the <ALONG.WITH> argument even if
  #a value for the <LENGTH.OUT> argument is set. If you want to specify the output length
  #with the <LENGTH.OUT> argument you must miss out the <ALONG.WITH> argument altogether

  if(!is.null(ALONG.WITH)){ 
    output.vector <- seq(from=FROM,by=BY,length.out=LENGTH.OUT,along.with=ALONG.WITH)
  }else{
    output.vector <- seq(from=FROM,by=BY,length.out=LENGTH.OUT)
  }
 
  return(output.vector)

}
#ASSIGN FUNCTION
# seqDS.o 

