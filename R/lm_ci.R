#' Linear Model Confidence Interval
#' 
#' Due to the shameful lack of a confidence interval in base `R` `lm()` output,
#' this function was created to take a model and produce a summary table of
#' coefficients including confidence interval estimates.
#'
#' @param model a model object created by `lm()`
#' @param expcoef a logical indicating whether or not to exponentiate the 
#'   coefficients and other estimates. Generally used if the model has a 
#'   log-transformed response. Defaults to `FALSE`.
#' @param robust a logical indicating whether or not to use the robust
#'   (sandwich) variance estimator when computing the standard errors
#'   of the parameter estimates. Defaults to `FALSE`.
#'
#' @return a matrix containing the parameter estimates, confidence
#'   interval, test statistic, and corresponding t-value.
#' @export
lm_ci <- function( model, expcoef=FALSE, robust=FALSE ){
  coef <- summary( model )$coef[,1]
  se <- ifelse1( robust, robust.se.lm(model)[,2], summary( model )$coef[,2] )
  tvalue <- coef / se
  pvalue <- 2*(1-pt(abs(tvalue), model$df.residual))
  if( expcoef ){
    ci95.lo <- exp( coef - qt(.975, model$df.residual) * se )
    ci95.hi <- exp( coef + qt(.975, model$df.residual) * se )
    est <- exp( coef )
  }
  else{
    ci95.lo <- coef - qt(.975, model$df.residual) * se
    ci95.hi <- coef + qt(.975, model$df.residual) * se
    est <- coef
  }
  rslt <- round( cbind( est, ci95.lo, ci95.hi, tvalue, pvalue ), 4 )
  colnames( rslt ) <- ifelse1( 	robust, 	
                                c("Est", "robust ci95.lo", "robust ci95.hi", "robust t value", "robust Pr(>|t|)"),
                                c("Est", "ci95.lo", "ci95.hi", "t value", "Pr(>|t|)") )			
  colnames( rslt )[1] <- ifelse( expcoef, "exp( Est )", "Est" )
  rslt
}


#' Generalized Linear Model Confidence Interval
#' 
#' Due to the shameful lack of a confidence interval in base `R` `glm()` output,
#' this function was created to take a model and produce a summary table of
#' coefficients including confidence interval estimates.
#'
#' @param model a model object created by `glm()`
#' @param transform a logical indicating whether or not to transform the 
#'   coefficients and other estimates via the link function. Defaults to `TRUE`.
#' @param robust a logical indicating whether or not to use the robust
#'   (sandwich) variance estimator when computing the standard errors
#'   of the parameter estimates. Defaults to `FALSE`.
#'
#' @return a matrix containing the parameter estimates, confidence
#'   interval, test statistic, and corresponding t-value.
#' @export
glm_ci <- function( model, transform=TRUE, robust=FALSE ){
  link <- model$family$link
  coef <- summary( model )$coef[,1]
  se <- ifelse1( robust, robust.se.glm(model)[,2], summary( model )$coef[,2] )
  zvalue <- coef / se
  pvalue <- 2*(1-pnorm(abs(zvalue)))
  
  if( transform & is.element(link, c("logit","log")) ){
    ci95.lo <- exp( coef - qnorm(.975) * se )
    ci95.hi <- exp( coef + qnorm(.975) * se )
    est <- exp( coef )
  }
  else{
    ci95.lo <- coef - qnorm(.975) * se
    ci95.hi <- coef + qnorm(.975) * se
    est <- coef
  }
  rslt <- round( cbind( est, ci95.lo, ci95.hi, zvalue, pvalue ), 4 )
  colnames( rslt ) <- ifelse1( 	robust, 	
                                c("Est", "robust ci95.lo", "robust ci95.hi", "robust z value", "robust Pr(>|z|)"),
                                c("Est", "ci95.lo", "ci95.hi", "z value", "Pr(>|z|)") )			
  colnames( rslt )[1] <- ifelse( transform & is.element(link, c("logit","log")), "exp( Est )", "Est" )
  rslt
}




