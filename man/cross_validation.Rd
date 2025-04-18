% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cross_validation.R
\name{cross_validation}
\alias{cross_validation}
\title{Assign Cross-Validation Folds for Labelled and Unlabelled Data}
\usage{
cross_validation(N, K, A)
}
\arguments{
\item{N}{Integer. Total number of observations in the dataset.}

\item{K}{Integer. Number of folds to assign for cross-validation.}

\item{A}{Vector. Treatment assignment vector, may contain \code{NA} for unlabelled samples.}
}
\value{
A list containing:
\describe{
\item{R}{Binary vector of length \code{N}. 1 indicates labelled data (non-missing \code{A}), 0 indicates unlabelled data.}
\item{foldid}{Integer vector of length \code{N}. Fold assignments (1 to \code{K}) for each observation, for use in cross-validation.}
}
}
\description{
Creates fold assignments for both labelled and unlabelled data using stratified random sampling,
ensuring an approximately equal number of samples per fold within each group.
}
\details{
This function supports semi-supervised learning workflows by separating observations into labelled and unlabelled groups
based on the presence of treatment labels (\code{A}). It assigns fold indices for both groups independently, allowing
balanced cross-fitting or validation strategies within each subgroup.
}
