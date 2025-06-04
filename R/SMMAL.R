#' Estimate Average Treatment Effect (ATE) via Semi-Supervised Learning Pipeline
#'
#' Executes a full semi-supervised ATE estimation pipeline. This includes cross-validation
#' fold assignment, feature selection via adaptive LASSO, model fitting using a specified learner
#' (e.g., bspline, xgboost, or random forest), and doubly robust ATE estimation
#' that leverages both labelled and unlabelled data.
#'
#' @param Y Numeric vector. Outcome variable (may contain \code{NA} for unlabelled observations).
#' @param A Numeric vector. Treatment indicator (1 = treated, 0 = control). May contain \code{NA} for unlabelled observations.
#' @param S Matrix or data frame. Surrogate variables used only in imputation models.
#' @param X Matrix or data frame. Main covariates used for outcome and propensity score modeling.
#' @param nfold Integer. Number of cross-validation folds. Default is 5.
#' @param cf_model Character string. Modeling method to use in cross-fitting. One of \code{"bspline"}, \code{"xgboost"}, or \code{"randomforest"}. Default is "bspline".
#' @param custom_model_fun A logical or function. If \code{NULL} or \code{FALSE}, bypasses adaptive-LASSO feature selection. Otherwise, enables two-stage tuning inside \code{compute_parameter()}.
#'
#' @return A list containing:
#' \describe{
#'   \item{est}{Estimated Average Treatment Effect (ATE).}
#'   \item{se}{Estimated standard error of the ATE.}
#' }
#'
#' @details
#' The pipeline first selects important covariates via adaptive LASSO. Then, it fits nuisance functions
#' (outcome regressions and propensity scores) using cross-fitting with the specified learner.
#' Finally, it applies a doubly robust estimator that integrates information from both labelled and
#' unlabelled observations to estimate the ATE.
#'
#' @importFrom stats coef predict quantile sd
#' @importFrom utils head
#'
#' @seealso \code{\link{cf}}, \code{\link{compute_parameter}}, \code{\link{cross_validation}}, \code{\link{ate.SSL}}
#'
#' @export



SMMAL <- function(Y, A, S, X, nfold = 5, cf_model = "bspline",custom_model_fun=NULL) {

  N=length(Y)

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
  result_parameter <- compute_parameter(nfold, Y, A, X, S, W, foldid, R, cf_model,custom_model_fun)

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
