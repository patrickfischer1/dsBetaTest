#' @title
#' glmDS1
#'
#' @description
#' This is the first serverside function called by ds.glm. It is an aggregation function that sets up the model structure
#' and creates the starting beta.vector that feeds, via ds.glm, into glmDS2 to enable iterative fitting of the generalized linear model that has been
#' been specified. For more details please see the extensive header for ds.glm.
#'
#' @export
#'
glmDS1.b<-function (formula, family, weights, data) {

errorMessage="No errors"

  # get the value of the 'data' and 'weights' parameters provided as character on the client side
  if(is.null(data)){
    dataTable <- NULL 
  }else{
    dataTable <- eval(parse(text=data))
  }
   
   formulatext <- Reduce(paste, deparse(formula))
   originalFormula <- formulatext
  
# Convert formula string into separate variable names split by |
  formulatext <- gsub(" ", "", formulatext, fixed=TRUE)
  formulatext <- gsub("~", "|", formulatext, fixed=TRUE)
  formulatext <- gsub("+", "|", formulatext, fixed=TRUE)
  formulatext <- gsub("*", "|", formulatext, fixed=TRUE)
  formulatext <- gsub("||", "|", formulatext, fixed=TRUE)

   formula2use <- as.formula(paste0(Reduce(paste, deparse(originalFormula)))) # here we need the formula as a 'call' object
   mod.glm.ds <- glm(formula2use, family=family, x=TRUE, control=glm.control(maxit=1), contrasts=NULL, data=dataTable)

  
#Remember model.variables and then varnames INCLUDE BOTH yvect AND linear predictor components 
	model.variables <- unlist(strsplit(formulatext, split="|", fixed=TRUE))
 
	 varnames <- c()
	  for(i in 1:length(model.variables)){
	    elt <- unlist(strsplit(model.variables[i], split="$", fixed=TRUE))
	    if(length(elt) > 1){
	      assign(elt[length(elt)], eval(parse(text=model.variables[i])))
	      originalFormula <- gsub(model.variables[i], elt[length(elt)], originalFormula, fixed=TRUE)
	      varnames <- append(varnames, elt[length(elt)])
	    }else{
	      varnames <- append(varnames, elt)
	    }
	  }

	varnames <- unique(varnames) 

   X.mat <- as.matrix(mod.glm.ds$x)
  
   dimX<-dim((X.mat))
   
 
   y.vect<-as.vector(mod.glm.ds$y)
   


 ##########################


filter.threshold.tab<-DANGER.nfilter.tab
filter.threshold.glm<-DANGER.nfilter.glm

 ##############################################################
 #FIRST TYPE OF DISCLOSURE TRAP - TEST FOR OVERSATURATED MODEL#
 ##############################################################

 glm.saturation.invalid<-0
 num.p<-dimX[2]
 num.N<-dimX[1]
   
 if(num.p>filter.threshold.glm*num.N){
 glm.saturation.invalid<-1
 errorMessage<-"FAILED: Model has too many parameters, there is a possible risk of disclosure - please simplify model"
 return(errorMessage) 
}
   
   
   coef.names<-names(mod.glm.ds$coefficients)

   if(is.null(weights)){
	w.vect<-rep(1,length(y.vect))
	}else{
	ftext <- paste0("cbind(",weights,")")
	w.vect <- eval(parse(text=ftext))
	}

################################
#SECOND TYPE OF DISCLOSURE TRAP#
################################
 
#If y, X or w data are invalid but user has modified clientside
#function (ds.glm) to circumvent trap, model will get to this point without
#giving a controlled shut down with a warning about invalid data.
#So as a safety measure, we will now use the same test that is used to
#trigger a controlled trap in the clientside function to destroy the
#score.vector and information.matrix in the study with the problem.



#CHECK Y VECTOR VALIDITY
	y.invalid<-0

	unique.values.y<-unique(y.vect)
	unique.values.noNA.y<-unique.values.y[complete.cases(unique.values.y)]

	if(length(unique.values.noNA.y)==2){
		tabvar<-table(y.vect)[table(y.vect)>=1]
		min.category<-min(tabvar)
		if(min.category<filter.threshold.tab){
		   y.invalid<-1
		   errorMessage<-"FAILED: y vector is binary with one category less than filter threshold for table cell size"
		   }
		}

#CHECK X MATRIX VALIDITY 
#Check no dichotomous X vectors with between 1 and filter.threshold 
#observations at either level 
	dimX<-dim((X.mat))

  	num.Xpar<-dimX[2]

	Xpar.invalid<-rep(0,num.Xpar)

  	for(pj in 1:num.Xpar){
	unique.values<-unique(X.mat[,pj])
	unique.values.noNA<-unique.values[complete.cases(unique.values)]

	if(length(unique.values.noNA)==2){
		tabvar<-table(X.mat[,pj])[table(X.mat[,pj])>=1]
		min.category<-min(tabvar)
		if(min.category<filter.threshold.tab){
		    Xpar.invalid[pj]<-1
		    errorMessage<-"FAILED: at least one column in X matrix is binary with one category less than filter threshold for table cell size"
            }
	   }
	}


#CHECK W VECTOR VALIDITY
	w.invalid<-0

	unique.values.w<-unique(w.vect)
	unique.values.noNA.w<-unique.values.w[complete.cases(unique.values.w)]

	if(length(unique.values.noNA.w)==2){
		tabvar<-table(w.vect)[table(w.vect)>=1]
		min.category<-min(tabvar)
		if(min.category<=filter.threshold.tab){
        w.invalid<-1
		errorMessage<-"FAILED: w vector is binary with one category less than filter threshold for table cell size"
            }
	}

#If y, X or w data are invalid, or the model is overparameterized, this will be detected by glmDS1
#and passed to ds.glm resulting in a warning and a controlled shut down of the function.
#But in case someone modifies the client side function to circumvent the trap, so the
#error is only apparent once the main main iterations have started via glmDS2
#the equivalent tests in glmDS2 will destroy the info.matrix and score.vector in the affected study so
#the model fitting will simply terminate.


return(list(dimX=dimX,coef.names=coef.names,y.invalid=y.invalid,Xpar.invalid=Xpar.invalid,w.invalid=w.invalid,
       glm.saturation.invalid=glm.saturation.invalid,errorMessage=errorMessage))
  
  
}
#glmDS1.b


