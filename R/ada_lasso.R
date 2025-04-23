#' Variable Selection Using LASSO
#'
#' This function performs variable selection using LASSO (Least Absolute Shrinkage and Selection Operator)
#' via cross-validated logistic regression (using `glmnet`). It returns the top `n` most important predictors
#' based on the absolute value of the LASSO coefficients.
#'
#' @param X A data frame or matrix of predictor variables.
#' @param Y A response vector (binary outcome: 0/1).
#'
#' @return A matrix containing only the `top_n` selected variables (columns) from the original predictor matrix `X`.
#'
#' @import glmnet
#' @export
ada_lasso <- function(X, Y) {
  observed_idx <- which(!is.na(Y))
  X_obs <- X[observed_idx, , drop = FALSE]
  Y_obs <- Y[observed_idx]

  X_matrix <- as.matrix(X_obs)
  Y_vector <- as.numeric(Y_obs)

  cv_fit1 <- cv.glmnet(X_matrix, Y_vector, alpha = 1, standardize = TRUE)
  beta_init <- as.vector(coef(cv_fit1, s = "lambda.min"))[-1]  # exclude intercept

  weights <- 1 / (abs(beta_init) + 1e-4)

  cv_fit2 <- cv.glmnet(X_matrix, Y_vector, alpha = 1, penalty.factor = weights, standardize = TRUE)
  beta_final <- as.vector(coef(cv_fit2, s = "lambda.min"))[-1]

  top_vars <- which(beta_final != 0)

  return(X[, top_vars, drop = FALSE])
}

