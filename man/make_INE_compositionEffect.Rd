% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_INE_compositionEffect.R
\name{make_INE_compositionEffect}
\alias{make_INE_compositionEffect}
\title{Returns a function which calculates Composition Effect of INE}
\usage{
make_INE_compositionEffect(reg, income_var, performance_var)
}
\arguments{
\item{reg}{a glm binary logistic regression object}

\item{income_var}{A character vector with the name of the income variable,
as it appear in \code{all.vars(formula(reg))}.}

\item{performance_var}{A character vector with the name of the schooling
performance variable, as it appear in \code{all.vars(formula(reg))}.}
}
\value{
Returns a bivariate function which calculates the INE Composition effect
  between two points/observations on the income distribution.
}
\description{
Returns a function which calculates Composition Effect of INE
}
