#' Estimate Nuisance Parameters for Semi-Supervised ATE Estimation
#'
#' Computes nuisance functions including conditional expectations and propensity scores
#' using cross-fitting, separately for labelled and unlabelled data. These estimates
#' are essential inputs for doubly robust or semi-supervised average treatment effect (ATE) estimators.
#'
#' @param nfold Integer. Number of cross-fitting folds.
#' @param Y Numeric vector. Outcome variable. Can contain \code{NA}s for unlabelled observations.
#' @param A Numeric vector. Treatment assignment indicator (0 or 1). Can contain \code{NA}s.
#' @param X Matrix or data frame. Covariates used for outcome and propensity score models.
#' @param S Matrix or data frame. Additional covariates used only in imputation models.
#' @param W Matrix or data frame. Combined set of covariates (typically \code{cbind(X, S)}).
#' @param foldid Integer vector. Fold assignments for cross-fitting.
#' @param R Binary vector. Label indicator: 1 = labelled (observed \code{A} and \code{Y}), 0 = unlabelled.
#' @param cf_model Function. A user-supplied cross-fitting wrapper function (e.g., based on Super Learner or other learners).
#' @param custom_model_fun A logical or function. If \code{NULL} or \code{FALSE}, bypasses adaptive-LASSO feature selection. Otherwise, enables two-stage tuning inside \code{compute_parameter()}.
#'
#' @return A named list of estimated nuisance parameters (each a numeric vector):
#' \describe{
#'   \item{pi1.bs}{Estimated propensity score \eqn{P(A = 1 \mid X)}.}
#'   \item{pi0.bs}{Estimated propensity score \eqn{P(A = 0 \mid X)} (computed as \code{1 - pi1.bs}).}
#'   \item{mu1.bs}{Estimated outcome regression \eqn{E[Y \mid A = 1, X]}.}
#'   \item{mu0.bs}{Estimated outcome regression \eqn{E[Y \mid A = 0, X]}.}
#'   \item{cap_pi1.bs}{Estimated imputed propensity score \eqn{P(A = 1 \mid W)}.}
#'   \item{cap_pi0.bs}{Estimated imputed propensity score \eqn{P(A = 0 \mid W)} (computed as \code{1 - cap_pi1.bs}).}
#'   \item{m1.bs}{Estimated imputed outcome regression \eqn{E[Y \mid A = 1, W]}.}
#'   \item{m0.bs}{Estimated imputed outcome regression \eqn{E[Y \mid A = 0, W]}.}
#' }
#'
#' @details
#' This function applies cross-fitting to estimate all required nuisance functions for
#' semi-supervised or doubly robust ATE estimators. Separate models are fit for the
#' labelled dataset and the full dataset (for imputation).
#'
#' @seealso \code{\link{cf}}
#'
#' @export


compute_parameter <- function(nfold,Y,A,X,S,W,foldid,R,cf_model,custom_model_fun){

  mu1.bs = cf(Y, X, nfold, R,foldid,cf_model,sub_set=A==1,custom_model_fun=custom_model_fun)

  mu0.bs = cf(Y, X, nfold, R,foldid,cf_model,sub_set=A==0,custom_model_fun=custom_model_fun)


  pi1.bs = cf(A, X, nfold, R,foldid,cf_model,custom_model_fun=custom_model_fun)
  # cap_pi1
  imp.A.bs = cf(A, W, nfold, R,foldid,cf_model,custom_model_fun=custom_model_fun)
  # m1
  imp.A1Y1.bs = cf(Y, W, nfold, R, foldid,cf_model,sub_set = A==1,custom_model_fun=custom_model_fun)
  # m0
  imp.A0Y1.bs = cf(Y, W, nfold, R, foldid,cf_model,sub_set = A==0,custom_model_fun=custom_model_fun)


  return(list(pi1.bs=pi1.bs$best_rounds_prediction,pi0.bs=1-pi1.bs$best_rounds_prediction,mu1.bs=mu1.bs$best_rounds_prediction,mu0.bs=mu0.bs$best_rounds_prediction,cap_pi1.bs=imp.A.bs$best_rounds_prediction,cap_pi0.bs=1-imp.A.bs$best_rounds_prediction, m1.bs=imp.A1Y1.bs$best_rounds_prediction, m0.bs=imp.A0Y1.bs$best_rounds_prediction))
}
