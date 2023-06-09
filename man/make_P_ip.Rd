% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_P_ip.R
\name{make_P_ip}
\alias{make_P_ip}
\title{Makes a bivariate function that produces predicted probabilities of a Logistic Regression (glm)}
\usage{
make_P_ip(reg, vars_of_interest)
}
\arguments{
\item{reg}{a glm binary logistic regression object}

\item{vars_of_interest}{A character vector with names of the schooling performance and income variables, as they appear in \code{all.vars(formula(reg))}. The other variables used in the glm estimation will be regarded as covariates.}
}
\value{
Returns a bivariate function which produces predicted values for \code{reg}, given the performance and income level passed to the function. All covariates are set to they mean (or mode)
}
\description{
A wrapper for producing predicted values for a logistic regression by returnin a
function with two inputs, performance and income, and setting all the other covariates
to their respective mean.
}
