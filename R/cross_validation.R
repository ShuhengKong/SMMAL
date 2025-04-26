#' Assign Cross-Validation Folds for Labelled and Unlabelled Data
#'
#' Creates fold assignments for both labelled and unlabelled data using stratified random sampling,
#' ensuring an approximately equal number of samples per fold within each group.
#'
#' @param N Integer. Total number of observations in the dataset.
#' @param nfold Integer. Number of folds to assign for cross-validation.
#' @param A Numeric vector. Treatment assignment indicator (may contain \code{NA} for unlabelled samples).
#' @param Y Numeric vector. Outcome variable (may contain \code{NA} for unlabelled samples).
#'
#' @return A list containing:
#' \describe{
#'   \item{R}{Binary vector of length \code{N}, where 1 indicates labelled observations (non-missing \code{A} and \code{Y}),
#'   and 0 indicates unlabelled observations.}
#'   \item{foldid}{Integer vector of length \code{N}. Fold assignments (from 1 to \code{nfold}) for use in cross-validation.}
#' }
#'
#' @details
#' The function first separates observations into labelled and unlabelled groups based on the
#' availability of both treatment (\code{A}) and outcome (\code{Y}). Within each group,
#' fold assignments are randomly assigned to ensure approximately balanced sample sizes
#' across folds. This setup supports semi-supervised learning workflows by maintaining
#' structure between labelled and unlabelled data during cross-fitting.
#'
#' @examples
#' set.seed(123)
#' N <- 100
#' A <- sample(c(0, 1, NA), size = N, replace = TRUE, prob = c(0.45, 0.45, 0.10))
#' Y <- sample(c(0, 1, NA), size = N, replace = TRUE, prob = c(0.45, 0.45, 0.10))
#'
#' # Assign 5 folds for cross-fitting
#' result <- cross_validation(N = N, nfold = 5, A = A, Y = Y)
#'
#' table(result$R)  # Check number of labelled vs unlabelled
#' table(result$foldid)  # Check how folds are distributed
#'
#'
#' @export


cross_validation <- function(N,nfold,A,Y){
  # Create the vector with 1 for non-missing values and 0 for missing values
  R <- ifelse(!is.na(A)& !is.na(Y), 1, 0)

  labelled_indices <- which(R == 1)
  unlabelled_indices <- which(R == 0)

  #make sure each fold has same ratio of labelled data
  foldid_labelled_values <- sample(rep_len(1:nfold, length.out = length(labelled_indices)))
  foldid_unlabelled_values <- sample(rep_len(1:nfold, length.out = length(unlabelled_indices)))

  foldid <- numeric(N)
  foldid_labelled <- numeric(N)

  foldid[labelled_indices] <- foldid_labelled_values
  foldid[unlabelled_indices] <- foldid_unlabelled_values

  return(list(R=R,foldid=foldid))
}
