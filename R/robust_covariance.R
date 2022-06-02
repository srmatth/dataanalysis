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