#' 
#' @title Computes the sum of each variable and the sum of products for each pair of variables
#' @description This function computes the sum of each vector of variable and the sum of the products of each two
#' variables (i.e. the scalar product of each two vectors).
#' @details computes the sum of each vector of variable and the sum of the products of each two
#' variables
#' @param x a character, the name of a vector, matrix or dataframe of variable(s) for which the covariance(s) and the
#' correlation(s) is (are) going to calculated for.
#' @param y NULL (default) or the name of a vector, matrix or dataframe with compatible dimensions to x. 
#' @param use a character string giving a method for computing covariances in the presence of missing values. 
#' This must be one of the strings "casewise.complete" or "pairwise.complete". If \code{use} is set to 
#' 'casewise.complete' then any rows with missing values are omitted from the vector, matrix or dataframe before the
#' calculations of the sums. If \code{use} is set to 'pairwise.complete' (which is the default case set on the client-side), 
#' then the sums of products are computed for each two variables using only the complete pairs of observations on the
#' two variables.
#' @return a list tha includes a matrix with elements the sum of products between each two variables, a matrix with
#' elements the sum of the values of each variable, a matrix with elements the number of complete cases in each
#' pair of variables, a list with the number of missing values in each variable separately (columnwise) and the number
#' of missing values casewise or pairwise depending on the arqument \code{use}, and an error message which indicates 
#' whether or not the input variables pass the disclosure control (i.e. none of them is dichotomous with a level
#' having less counts than the pre-specified threshold). If any of the input variables does not pass the disclosure
#' control then all the output values are replaced with NAs
#' @author Gaye A., Avraam D., Burton P., Fischer P.
#' @export
#' 
covDS.o <- function(x=NULL, y=NULL, use=NULL){
  
  #############################################################
  #MODULE 1: CAPTURE THE nfilter SETTINGS
  thr <- listDisclosureSettingsDS.o()
  nfilter.tab <- as.numeric(thr$nfilter.tab)
  #nfilter.glm <- as.numeric(thr$nfilter.glm)
  #nfilter.subset <- as.numeric(thr$nfilter.subset)
  #nfilter.string <- as.numeric(thr$nfilter.string)
  #############################################################
  
  # create a data frame for the variables
  if (is.null(y)){
    dataframe <- as.data.frame(x)
  }else{
    dataframe <- as.data.frame(cbind(x,y))
  }
  
  # names of the variables
  cls <- colnames(dataframe)
  
  # number of the input variables
  N.vars <- dim(dataframe)[2]
  
  
  ######################
  # DISCLOSURE CONTROL # 
  ######################
  
  # CHECK X MATRIX VALIDITY 
  # Check no dichotomous X vectors with between 1 and filter.threshold 
  # observations at either level 
  
  X.mat <- as.matrix(dataframe)
  
  dimX <- dim((X.mat))
  
  num.Xpar <- dimX[2]
  
  Xpar.invalid <- rep(0, num.Xpar)
  
  for(pj in 1:num.Xpar){
    unique.values.noNA <- unique((X.mat[,pj])[complete.cases(X.mat[,pj])]) 
    if(length(unique.values.noNA)==2){
      tabvar <- table(X.mat[,pj])[table(X.mat[,pj])>=1] #tabvar COUNTS N IN ALL CATEGORIES WITH AT LEAST ONE OBSERVATION
      min.category <- min(tabvar)
      if(min.category < nfilter.tab){
        Xpar.invalid[pj] <- 1
      }
    }
  }	
  
  # if any of the vectors in X matrix is invalid then the function returns all the
  # outputs by replacing their values with NAs
  
  if(is.element('1', Xpar.invalid)==TRUE){
    
    sums.of.products <- matrix(NA, ncol=N.vars, nrow=N.vars)
    rownames(sums.of.products) <- cls
    colnames(sums.of.products) <- cls 
    
    sums <- matrix(NA, ncol=1, nrow=N.vars)
    rownames(sums) <- cls
    
    complete.counts <- matrix(NA, ncol=N.vars, nrow=N.vars)
    rownames(complete.counts) <- cls
    colnames(complete.counts) <- cls 
    
    column.NAs <- matrix(NA, ncol=N.vars, nrow=1)
    colnames(column.NAs) <- cls
    
    casewise.NAs <- matrix(NA, ncol=1, nrow=1)
    
    pairwise.NAs <- matrix(NA, ncol=N.vars, nrow=N.vars)
    rownames(pairwise.NAs) <- cls
    colnames(pairwise.NAs) <- cls 
    
    if (use=='casewise.complete'){
      na.counts <- list(column.NAs, casewise.NAs)
      names(na.counts) <- list(paste0("Number of NAs in each column"), paste0("Number of NAs casewise"))
    }
    if (use=='pairwise.complete'){
      na.counts <- list(column.NAs, pairwise.NAs)
      names(na.counts) <- list(paste0("Number of NAs in each column"), paste0("Number of NAs pairwise"))
    }
    
    errorMessage <- "ERROR: at least one variable is binary with one category less than the filter threshold for table cell size"
    
  }	
  
  # if all vectors in X matrix are valid then the output matrices are calculated
  
  if(is.element('1', Xpar.invalid)==FALSE){
    
    if (use=='casewise.complete'){
      
      # calculate the number of NAs in each variable separately
      column.NAs <- matrix(ncol=N.vars, nrow=1)
      colnames(column.NAs) <- cls
      for(i in 1:N.vars){
        column.NAs[1,i] <- length(dataframe[,i])-length(dataframe[complete.cases(dataframe[,i]),i])
      }
      
      # if use is casewise.complete first remove any rows from the dataframe that include NAs
      casewise.dataframe <- dataframe[complete.cases(dataframe),]
      
      # calculate the number of NAs casewise
      casewise.NAs <- as.matrix(dim(dataframe)[1]-dim(casewise.dataframe)[1])
      
      # counts for NAs to be returned to the client:
      # This is a list with (a) a vector with the number of NAs in each variable (i.e. in each column) 
      # separately and (b) the number of NAs casewise (i.e. the number of rows deleted from the input dataframe 
      # which are the rows that at least one of their cells includes missing value)
      na.counts <- list(column.NAs, casewise.NAs)
      names(na.counts) <- list(paste0("Number of NAs in each column"), paste0("Number of NAs casewise"))
      
      # A matrix with elements the sum of products between each two variables
      sums.of.products <- matrix(ncol=N.vars, nrow=N.vars)
      rownames(sums.of.products) <- cls
      colnames(sums.of.products) <- cls 
      for(m in 1:N.vars){
        for(p in 1:N.vars){
          sums.of.products[m,p] <- sum(as.numeric(as.character(casewise.dataframe[,m]))*as.numeric(as.character(casewise.dataframe[,p])))
        }
      }
      
      # A matrix with elements the sum of each variable 
      sums <- matrix(ncol=1, nrow=N.vars)
      rownames(sums) <- cls
      for(m in 1:N.vars){
        sums[m,1] <- sum(as.numeric(as.character(casewise.dataframe[,m])))      
      }
      
      # A matrix with elements the sum of squares of each variable after removing missing values casewise
      sums.of.squares <- matrix(ncol=N.vars, nrow=N.vars)
      rownames(sums.of.squares) <- cls
      colnames(sums.of.squares) <- cls 
      for(m in 1:N.vars){
        for(p in 1:N.vars){
          sums.of.squares[m,p] <- sum(as.numeric(as.character(casewise.dataframe[,m]))*as.numeric(as.character(casewise.dataframe[,m])))
        }
      }
      
      # Calculate the variance of each variable after removing missing values casewise
      vars <- matrix(ncol=1, nrow=N.vars)
      rownames(sums) <- cls
      for(m in 1:N.vars){
        vars[m,1] <- var(as.numeric(as.character(casewise.dataframe[,m])))
      }		
      
      complete.counts <- matrix(dim(casewise.dataframe)[1], ncol=N.vars, nrow=N.vars)
      rownames(complete.counts) <- cls
      colnames(complete.counts) <- cls 
      
    } 
    
    if (use=='pairwise.complete'){
      
      
      cmpCases <- cmpCases <- matrix(nrow = dim(x)[1], ncol = dim(x)[2])
      for(i in 1:N.vars) {
        
        cmpCases[, i] <- complete.cases(dataframe[, i])
      }
      
      column.NAs <- colSums(!cmpCases)
      column.NAs <- as.matrix(t(column.NAs))
      
      colnames(column.NAs) <- cls
      
      # calculate the number of NAs in each pair of variables
      # calculate the number of complete cases in each pair of variables
      # calculate the sum of products for each pair of variables
      pairwise.NAs <- matrix(ncol=N.vars, nrow=N.vars)
      complete.counts <- matrix(ncol=N.vars, nrow=N.vars)
      sums.of.products <- matrix(ncol=N.vars, nrow=N.vars)
      for(i in 1:N.vars){
        for(j in i:N.vars){
          # Identifying complete cases
          pairWiseCases = cmpCases[, i] & cmpCases[, j]
          
          # Number of NAs is negation of pairwiseCases
          pairwise.NAs[i,j] <- sum(!pairWiseCases)
          pairwise.NAs[j,i] <- sum(!pairWiseCases)
          
          # Complete cases as sum of logical vector
          complete.counts[i,j] <- sum(pairWiseCases)
          complete.counts[j,i] <- sum(pairWiseCases)
          
          # Calulating pairwise product
          sumOfProduct <- sum(x[pairWiseCases, i] * x[pairWiseCases, j])
          sums.of.products[i,j] <- sumOfProduct
          sums.of.products[j,i] <- sumOfProduct
        } 
      } 
      
      # counts for NAs to be returned to the client:
      # This is a list with (a) a vector with the number of NAs in each variable (i.e. in each column) 
      # separately and (b) a matrix with the number of NAs in each pair of variables
      na.counts <- list(column.NAs, pairwise.NAs)
      names(na.counts) <- list(paste0("Number of NAs in each column"), paste0("Number of NAs pairwise"))
      
      
      # A matrix with elements the sum, sum of squares and variance of each variable
      sums <- matrix(ncol=N.vars, nrow=1)
      sums.of.squares <- matrix(ncol=N.vars, nrow=1)
      vars <- matrix(ncol=N.vars, nrow=1)
      for(m in 1:N.vars){
        sums[1, m] <- sum(x[cmpCases[, m], m])
        sums.of.squares[1, m] <- sum(x[cmpCases[, m], m] ^ 2)
        vars[1, m] <- var(x[cmpCases[, m], m])
      }
      
      # Replication of row to have matrix
      sums <- matrix(rep(sums,each = N.vars), nrow = N.vars, byrow = TRUE)
      sums.of.squares <- matrix(rep(sums.of.squares,each = N.vars), nrow = N.vars, byrow = TRUE)
      vars <- matrix(rep(vars,each = N.vars), nrow = N.vars, byrow = TRUE)
      
      
      # Assigning variable names as row/colnames of results
      rownames(sums.of.squares) <- cls
      colnames(sums.of.squares) <- cls
      rownames(sums) <- cls
      colnames(sums) <- cls
      rownames(complete.counts) <- cls
      colnames(complete.counts) <- cls
      rownames(sums.of.products) <- cls
      colnames(sums.of.products) <- cls 
      rownames(pairwise.NAs) <- cls
      colnames(pairwise.NAs) <- cls
    }
    
    # if all vectors in X matrix are valid then a NA error message is returned
    errorMessage <- NA 
    
  }  
  
  return(list(sums.of.products=sums.of.products, sums=sums, complete.counts=complete.counts, na.counts=na.counts, errorMessage=errorMessage, vars=vars, sums.of.squares=sums.of.squares))
  
}
#AGGREGATE FUNCTION
# covDS.o
