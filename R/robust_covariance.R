#' Robust Variance computation for GLMs
#'
#' @param glm.obj a model created by `glm()`
#'
#' @return robust covariance matrix
robust.vcov.glm <- function(glm.obj){
  if (is.matrix(glm.obj$x)) 
    xmat<-glm.obj$x
  else {
    mf<-model.frame(glm.obj)
    xmat<-model.matrix(terms(glm.obj),mf)		
  }
  umat <- residuals(glm.obj,"working")*glm.obj$weights*xmat
  modelv<-summary(glm.obj)$cov.unscaled
  robust.cov <- modelv%*%(t(umat)%*%umat)%*%modelv
  dimnames( robust.cov ) <- dimnames( vcov(glm.obj) )
  return( robust.cov )
}

#' Robust variance computation for LM
#'
#' @param lm.obj a model created by `lm()`
#'
#' @return the robust covariance matrix
robust.vcov.lm <- function( lm.obj ){
  X <- model.matrix( lm.obj )
  eps <- lm.obj$residuals
  robust.cov <- solve( t(X)%*%X ) %*%( t(X) %*% diag(eps^2) %*% X ) %*% solve( t(X)%*%X )
  dimnames( robust.cov ) <- dimnames( vcov(lm.obj) )
  return( robust.cov )
}


#' Robust standard error computation for LM
#'
#' @param model a model object created by `lm()`
#'
#' @return a matrix containing the robust standard errors and confidence intervals
robust.se.lm <- function( model) { 
  s <- summary( model) 
  X <- model.matrix( model )
  sandwich.cov <- robust.vcov.lm( model )
  sand.se <- sqrt( diag( sandwich.cov )) 
  t <- model$coefficients/sand.se
  p <- 2*pt( -abs( t ), dim(X)[1]-dim(X)[2] ) 
  ci95.lo <- model$coefficients - qt( .975, dim(X)[1]-dim(X)[2] ) * sand.se
  ci95.hi <- model$coefficients + qt( .975, dim(X)[1]-dim(X)[2] ) * sand.se
  rslt <- cbind( model$coefficients, sand.se, ci95.lo, ci95.hi, t, p ) 
  dimnames(rslt)[[2]] <- c( dimnames( s$coefficients )[[2]][1], "Robust SE", "ci95.lo", "ci95.hi", dimnames( s$coefficients )[[2]][3:4] ) 
  rslt 
}

#' Robust standard error computation for GLM
#'
#' @param glm.obj a model object created by `glm()`
#'
#' @return a matrix containing the robust standard errors and confidence intervals
robust.se.glm <- function(glm.obj){
  ## 	Compute robust (sandwich) variance estimate
  robust.cov <- robust.vcov.glm(glm.obj)
  
  ##	Format the model output with p-values and CIs
  s <- summary( glm.obj) 
  robust.se <- sqrt( diag( robust.cov )) 
  z <- glm.obj$coefficients/robust.se
  p <- 2*pnorm( -abs( z ) ) 
  ci95.lo <- glm.obj$coefficients - qnorm( .975 ) * robust.se
  ci95.hi <- glm.obj$coefficients + qnorm( .975 ) * robust.se
  rslt <- cbind( glm.obj$coefficients, robust.se, ci95.lo, ci95.hi, z, p ) 
  dimnames(rslt)[[2]] <- c( dimnames( s$coefficients )[[2]][1], "Robust SE", "ci95.lo", "ci95.hi", "z value", "Pr(>|z|)" ) 
  rslt 
}




