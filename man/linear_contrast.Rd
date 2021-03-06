% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linear_contrast.R
\name{linear_contrast}
\alias{linear_contrast}
\title{Linear Contrast Function}
\usage{
linear_contrast(
  contr.names,
  contr.coef = rep(1, length(contr.names)),
  model,
  transform = TRUE,
  robust = FALSE,
  verbose = TRUE
)
}
\arguments{
\item{contr.names}{The names of the parameters included in the contrast}

\item{contr.coef}{The contrast coefficients, the order corresponds to
the order of \code{contr.names}.}

\item{model}{A model object created by \code{lm()} or \code{glm()}}

\item{transform}{Logical indicating whether the output should be
transformed via the link function. Default is \code{TRUE}.}

\item{robust}{Logical indicating whether the robust (Sandwich)
variance estimator should be used in the computation of the standard
errors. Default is \code{FALSE}.}

\item{verbose}{Logical indicating whether the function should print out
a description of the test that it is doing.}
}
\value{
A data frame containing the point estimate, standard error,
test statistic, p-value, and confidence interval corresponding to
the linear contrast.
}
\description{
Computes estimates, standard errors, confidence intervals
and hypothesis tests for linear contrasts of model coefficients
in a generalized linear model or regular linear model.
}
