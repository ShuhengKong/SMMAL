
#' Estimate Conditional Expectations and Propensities for Semi-Supervised ATE
#'
#' Computes nuisance parameters such as conditional expectations and propensity scores
#' using cross-fitting for both labelled and unlabelled data. This function is central
#' for ATE estimation under semi-supervised or doubly robust frameworks.
#'
#' @param nfold Integer. Number of cross-fitting folds.
#' @param Y Numeric vector. Outcome variable (can contain NAs for unlabelled data).
#' @param A Numeric vector. Treatment assignment (0 or 1, can contain NAs).
#' @param X Matrix or data frame. Covariates used for outcome and propensity estimation.
#' @param S Matrix or data frame. Additional covariates only used in imputation models.
#' @param W Matrix or data frame. Combination of X and S.
#' @param foldid Integer vector. Fold assignments for cross-fitting.
#' @param R Binary vector. 1 indicates labelled data (non-missing A and Y), 0 indicates unlabelled.
#' @param cf_model Function. Cross-fitting wrapper function, e.g., using Super Learner or another learner.
#'
#' @return A list containing estimated nuisance parameters (each is a vector):
#' \describe{
#'   \item{pi1.bs}{Estimated propensity score \eqn{P(A = 1 | X)}}
#'   \item{pi0.bs}{Estimated propensity score \eqn{P(A = 0 | X)} = 1 - pi1.bs}
#'   \item{mu1.bs}{Estimated outcome regression \eqn{E[Y | A = 1, X]}}
#'   \item{mu0.bs}{Estimated outcome regression \eqn{E[Y | A = 0, X]}}
#'   \item{cap_pi1.bs}{Estimated imputed propensity score \eqn{P(A = 1 | W)}}
#'   \item{cap_pi0.bs}{Estimated imputed propensity score \eqn{P(A = 0 | W)} = 1 - cap_pi1.bs}
#'   \item{m1.bs}{Estimated imputed outcome regression \eqn{E[Y | A = 1, W]}}
#'   \item{m0.bs}{Estimated imputed outcome regression \eqn{E[Y | A = 0, W]}}
#' }
#' @export




compute_parameter <- function(nfold,Y,A,X,S,W,foldid,R,cf_model){

  mu1.bs = cf(Y, X, nfold, R,foldid,cf_model,sub_set=A==1)

  mu0.bs = cf(Y, X, nfold, R,foldid,cf_model,sub_set=A==0)


  pi1.bs = cf(A, X, nfold, R,foldid,cf_model)
  # cap_pi1
  imp.A.bs = cf(A, W, nfold, R,foldid,cf_model)
  # m1
  imp.A1Y1.bs = cf(Y, W, nfold, R, foldid,cf_model,sub_set = A==1)
  # m0
  imp.A0Y1.bs = cf(Y, W, nfold, R, foldid,cf_model,sub_set = A==0)


  return(list(pi1.bs=pi1.bs$best_rounds_prediction,pi0.bs=1-pi1.bs$best_rounds_prediction,mu1.bs=mu1.bs$best_rounds_prediction,mu0.bs=mu0.bs$best_rounds_prediction,cap_pi1.bs=imp.A.bs$best_rounds_prediction,cap_pi0.bs=1-imp.A.bs$best_rounds_prediction, m1.bs=imp.A1Y1.bs$best_rounds_prediction, m0.bs=imp.A0Y1.bs$best_rounds_prediction))
}
