#' Summarize a multinomial Model Fit
#' 
#' This function summarizes each level of a multinomial model
#' fit as compared to the first level.
#'
#' @param model a multinomial model fit with `nnet::multinom()`
#'
#' @return a summary of the multinomial model fit
#' @export
multinom_summ <- function( model ){
  s <- summary( model )
  for( i in 1:length(model$coef) ){
    cat( "\nLevel ", model$lev[i+1],  "vs. Level ", model$lev[1], "\n" )
    coef <- s$coefficients[i,]
    rrr <- exp( coef )
    se <- s$standard.errors[i,]
    zStat <- coef / se
    pVal <- 2*pnorm( abs(zStat), lower.tail=FALSE )
    ci95.lo <- exp( coef - qnorm(.975)*se )
    ci95.hi <- exp( coef + qnorm(.975)*se )
    rslt <- cbind( rrr, se, zStat, pVal, ci95.lo, ci95.hi )
    print( round( rslt, 3 ) )
  }
}