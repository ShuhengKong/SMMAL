% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/random_ate.R
\name{ate.SSL}
\alias{ate.SSL}
\title{Estimate ATE using SSL}
\usage{
ate.SSL(
  Y,
  A,
  mu1,
  mu0,
  pi1,
  pi0,
  imp.A,
  imp.A1Y1,
  imp.A0Y1,
  min.pi = 0.05,
  max.pi = 0.95
)
}
\arguments{
\item{Y}{Outcome vector}

\item{A}{Treatment assignment (0/1)}

\item{mu1}{Estimated outcome under treatment}

\item{mu0}{Estimated outcome under control}

\item{pi1}{Estimated propensity score for A = 1}

\item{pi0}{Estimated propensity score for A = 0}

\item{imp.A}{Estimated propensity score using covariates W}

\item{imp.A1Y1, imp.A0Y1}{Nuisance model predictions}

\item{min.pi}{Minimum value for pi to avoid division by zero}

\item{max.pi}{Maximum value for pi to avoid division by zero}
}
\value{
A list with ATE estimate and standard error
}
\description{
Estimate ATE using SSL
}
