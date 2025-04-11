#' Assign Cross-Validation Folds for Labelled and Unlabelled Data
#'
#' Creates fold assignments for both labelled and unlabelled data using stratified random sampling,
#' ensuring an approximately equal number of samples per fold within each group.
#'
#' @param N Integer. Total number of observations in the dataset.
#' @param K Integer. Number of folds to assign for cross-validation.
#' @param A Vector. Treatment assignment vector, may contain \code{NA} for unlabelled samples.
#'
#' @return A list containing:
#' \describe{
#'   \item{R}{Binary vector of length \code{N}. 1 indicates labelled data (non-missing \code{A}), 0 indicates unlabelled data.}
#'   \item{foldid}{Integer vector of length \code{N}. Fold assignments (1 to \code{K}) for each observation, for use in cross-validation.}
#' }
#'
#' @details
#' This function supports semi-supervised learning workflows by separating observations into labelled and unlabelled groups
#' based on the presence of treatment labels (\code{A}). It assigns fold indices for both groups independently, allowing
#' balanced cross-fitting or validation strategies within each subgroup.
#'
#' @export
#'

#function role:cross_validation
cross_validation <- function(N,K,A){
  # Create the vector with 1 for non-missing values and 0 for missing values
  R <- ifelse(!is.na(A), 1, 0)

  labelled_indices <- which(R == 1)
  unlabelled_indices <- which(R == 0)

  #make sure each fold has same ratio of labelled data
  foldid_labelled_values <- sample(rep_len(1:K, length.out = length(labelled_indices)))
  foldid_unlabelled_values <- sample(rep_len(1:K, length.out = length(unlabelled_indices)))

  foldid <- numeric(N)
  foldid_labelled <- numeric(N)

  foldid[labelled_indices] <- foldid_labelled_values
  foldid[unlabelled_indices] <- foldid_unlabelled_values

  return(list(R=R,foldid=foldid))
}
