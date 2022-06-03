#' Collapse Grouped Binary Data
#' 
#' This function can be used to take binary data and collapse it
#' into binomial data. Occassionally useful.
#'
#' @param data the data frame containing binary data
#' @param outcome the binary response variable
#'
#' @return a data frame in the binomial format
#' @export
binary_collapse <- function( data, outcome ){
  index <- (1:length(names(data)))[ names(data)==outcome ]
  y <- data[,index]
  covnames <- names( data )[-index]
  data <- data[,-index]
  if( is.null( dim( data ) ) ){
    rslt <- aggregate( y, list(data), FUN=length)
    rslt <- as.data.frame( cbind( rslt, aggregate( y, list(data), FUN=sum)[dim(rslt)[2]] ) )
  }
  else{
    rslt <- aggregate( y, data, FUN=length)
    rslt <- as.data.frame( cbind( rslt, aggregate( y, data, FUN=sum)[dim(rslt)[2]] ) )
  }
  names( rslt ) <- c( covnames, "n", paste("n.", outcome, sep="") )
  rslt
}


#' Binary Goodness of Fit Test
#'
#' @param fit a model object fir with `glm()`
#' @param ngrp the number of groups for the binary test. Defaults to 10.
#' @param print.table Logical, defaults to `TRUE`. Prints results as well as
#'   returning them.
#'
#' @return test results
#' @export
binary_gof <- function( fit, ngrp=10, print.table=TRUE ){
  y <- fit$y
  phat <- fitted( fit )
  fittedgrps <- cut( phat, quantile( phat, seq(0,1,by=1/ngrp) ), include.lowest=TRUE )
  n <- aggregate( y, list( fittedgrps ), FUN=length )[,2]
  Obs <- aggregate( y, list( fittedgrps ), FUN=sum )[,2]
  Exp <- aggregate( phat, list( fittedgrps ), FUN=sum )[,2]
  if( print.table==TRUE ){
    cat( "\nFitted Probability Table:\n\n" )
    rslt <- as.data.frame( cbind( 1:ngrp, n, Obs, Exp ) )
    names( rslt )[1] <- "group"
    print( rslt )
  }
  chisqstat <- sum( (Obs - Exp)^2 / ( Exp*(1-Exp/n) ) )
  df <- ngrp - 2
  pVal <- pchisq( chisqstat, df, lower.tail=FALSE )
  cat( "\n Hosmer-Lemeshow GOF Test:\n\n" )
  cbind( chisqstat, df, pVal )
}



