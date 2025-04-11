#' Estimate Average Treatment Effect (ATE) via Semi-Supervised Pipeline
#'
#' Executes the full ATE estimation pipeline using a semi-supervised learning approach.
#' This includes cross-validation fold assignment, model fitting using a chosen learner (e.g., xgboost),
#' and doubly robust ATE estimation using both labelled and unlabelled data.
#'
#' @param N Integer. Total number of observations.
#' @param Y Numeric vector. Observed outcomes (may include NAs for unlabelled data).
#' @param A Numeric vector. Treatment indicator (1 for treated, 0 for control; may include NAs).
#' @param S Matrix or data frame. Auxiliary covariates used only in imputation (\eqn{N \times q}).
#' @param X Matrix or data frame. Main covariates used for outcome and propensity modeling (\eqn{N \times p}).
#' @param K Integer. Number of cross-validation folds. Default is 5.
#' @param top_n Integer. Number of top variables to select using LASSO. Default is 1000.
#' @param cf_model Character. The modeling method to use in cross-fitting.
#'
#' @importFrom stats coef predict quantile sd
#' @importFrom utils head
#'
#' @return A list containing:
#' \describe{
#'   \item{est}{Estimated Average Treatment Effect (ATE).}
#'   \item{se}{Standard error of the ATE estimate.}
#' }
#' @export
SMMAL <- function(N, Y, A, S, X, K = 5, top_n = 1000, cf_model = "bspline") {


  # Variable selection (adaptive LASSO)
  if (ncol(X) > top_n) {
    X <- ada_lasso(X, Y, top_n)
  }

  # Combine covariates for W
  W <- cbind(S, X)


  # Cross-validation setup
  cv_result <- cross_validation(N, K, A)
  foldid <- cv_result$foldid
  R <- cv_result$R

  # Model training and ATE computation
  result_parameter <- compute_parameter(K, Y, A, X, S, W, foldid, R, cf_model)

  # Extract results
  pi1.bs <- result_parameter$pi1.bs
  pi0.bs <- result_parameter$pi0.bs
  mu1.bs <- result_parameter$mu1.bs
  mu0.bs <- result_parameter$mu0.bs
  cap_pi1.bs <- result_parameter$cap_pi1.bs
  cap_pi0.bs <- result_parameter$cap_pi0.bs
  m1.bs <- result_parameter$m1.bs
  m0.bs <- result_parameter$m0.bs

  # ATE Estimation
  ate_result_SSL <- ate.SSL(Y, A, mu1.bs, mu0.bs, pi1.bs, pi0.bs, cap_pi1.bs, m1.bs, m0.bs)

  return(ate_result_SSL)
}
