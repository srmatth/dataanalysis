#' Linear Contrast Function
#' 
#' Computes estimates, standard errors, confidence intervals
#' and hypothesis tests for linear contrasts of model coefficients
#' in a generalized linear model or regular linear model.
#'
#' @param contr.names The names of the parameters included in the contrast
#' @param contr.coef The contrast coefficients, the order corresponds to
#'   the order of `contr.names`.
#' @param model A model object created by `lm()` or `glm()`
#' @param transform Logical indicating whether the output should be
#'   transformed via the link function. Default is `TRUE`.
#' @param robust Logical indicating whether the robust (Sandwich)
#'   variance estimator should be used in the computation of the standard
#'   errors. Default is `FALSE`.
#' @param verbose Logical indicating whether the function should print out
#'   a description of the test that it is doing.
#'
#' @return A data frame containing the point estimate, standard error,
#'   test statistic, p-value, and confidence interval corresponding to
#'   the linear contrast.
#' @export
linear_contrast <- function(
  contr.names, 
  contr.coef = rep(1, length(contr.names)),
  model, 
  transform = TRUE, 
  robust = FALSE,
  verbose = TRUE
) {
  ## Get model parameters
  beta.hat <- model$coef
  link <- model$family$link
  if (is.null(link)) link <- "none"
  
  ## get the covariance matrix for the parameters
  if (robust & (link != "none")) cov.beta <- robust.vcov.glm(model) 
  else if (robust) cov.beta <- robust.vcov.lm(model)
  else cov.beta <- vcov(model)
  
  ## Compute the contrasts
  contr.index <- match(contr.names, dimnames(cov.beta)[[1]])
  beta.hat <- beta.hat[contr.index]
  cov.beta <- cov.beta[contr.index, contr.index]
  est <- contr.coef %*% beta.hat
  
  ## Get the standard errors of the contrasts
  se.est <- sqrt(contr.coef %*% cov.beta %*% contr.coef)
  
  ## Compute statistics and CIs
  zStat <- est/se.est
  pVal <- 2 * pnorm(abs(zStat), lower.tail = FALSE)
  ci95.lo <- est - qnorm(0.975) * se.est
  ci95.hi <- est + qnorm(0.975) * se.est
  
  ## Deal with potential link functions
  if (transform & is.element(link, c("logit", "log"))) {
    ci95.lo <- exp(ci95.lo)
    ci95.hi <- exp(ci95.hi)
    est <- exp(est)
  }
  ## Print out H0 definition
  if (verbose) {
    cat("\nTest of H_0: ")
    for (i in 1:(length(contr.names) - 1)) {
      cat(contr.coef[i], "*", contr.names[i], " + ", sep = "")
    }
    cat(contr.coef[i + 1], "*", contr.names[i + 1], " = 0 :\n\n", sep = "")
  }
  
  ## Compile results and return
  rslt <- data.frame(est, se.est, zStat, pVal, ci95.lo, ci95.hi)
  colnames(rslt)[1] <- ifelse(
    transform && is.element(link, c("logit", "log")), 
    "exp( Est )", 
    "Est"
  )
  round(rslt, 3)
}