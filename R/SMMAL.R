#' Estimate Average Treatment Effect (ATE) via Semi-Supervised Pipeline
#'
#' Executes the full ATE estimation pipeline using a semi-supervised learning approach.
#' This includes cross-validation fold assignment, model fitting using a chosen learner (e.g., xgboost),
#' and doubly robust ATE estimation using both labelled and unlabelled data.
#'
#' @param Y Numeric vector. Observed outcomes (may include NAs for unlabelled data).
#' @param A Numeric vector. Treatment indicator (1 for treated, 0 for control; may include NAs).
#' @param S Matrix or data frame. Surrogate used only in imputation.
#' @param X Matrix or data frame. Main covariates used for outcome and propensity modeling.
#' @param nfold Integer. Number of cross-validation folds. Default is 5.
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
SMMAL <- function(Y, A, S, X, nfold = 5, cf_model = "bspline") {

  N=length(Y)

  if (any(is.na(Y) != is.na(A))) {
    warning("Each element of Y and A must be either both missing or both observed.")
  }

  X <- ada_lasso(X, Y)


  if (is.null(X) || ncol(X) == 0) {
    stop("Error: 'X' must be a non-empty data frame.")
  }

  # Combine covariates for W
  W <- cbind(S, X)

  if (any(is.na(Y) != is.na(A))) {
    warning("Each element of Y and A must be either both missing or both observed.")
  }

  # Cross-validation setup
  cv_result <- cross_validation(N, nfold, A, Y)
  foldid <- cv_result$foldid
  R <- cv_result$R

  cf_model = "bspline"
  # Model training and ATE computation
  result_parameter <- compute_parameter(nfold, Y, A, X, S, W, foldid, R, cf_model)

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
  ate_result_SSL <- ate.SSL(Y[R==1], A[R==1], mu1.bs, mu0.bs, pi1.bs, pi0.bs, cap_pi1.bs, m1.bs, m0.bs)

  return(ate_result_SSL)
}
