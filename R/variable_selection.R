#' Variable Selection Using LASSO
#'
#' This function performs variable selection using LASSO (Least Absolute Shrinkage and Selection Operator)
#' via cross-validated logistic regression (using `glmnet`). It returns the top `n` most important predictors
#' based on the absolute value of the LASSO coefficients.
#'
#' @param X A data frame or matrix of predictor variables.
#' @param Y A response vector (binary outcome: 0/1).
#' @param top_n An integer specifying the number of top variables to select based on LASSO coefficient magnitude.
#'
#' @return A matrix containing only the `top_n` selected variables (columns) from the original predictor matrix `X`.
#'
#' @import glmnet
#' @export
ada_lasso <- function(X, Y, top_n) {
  X_matrix <- as.matrix(X)

  if (is.null(colnames(X_matrix))) {
    colnames(X_matrix) <- paste0("V", 1:ncol(X_matrix))
  }

  cv_fit <- cv.glmnet(X_matrix, Y, family = "binomial", alpha = 1, standardize = TRUE)
  best_lambda <- cv_fit$lambda.min
  lasso_coefs <- coef(cv_fit, s = best_lambda)

  coef_df <- as.data.frame(as.matrix(lasso_coefs))
  coef_df$Variable <- rownames(coef_df)
  names(coef_df)[1] <- "Coefficient"

  coef_df <- coef_df[coef_df$Variable != "(Intercept)" & coef_df$Coefficient != 0, ]
  if (nrow(coef_df) == 0) {
    coef_df <- as.data.frame(as.matrix(lasso_coefs))
    coef_df$Variable <- rownames(coef_df)
    names(coef_df)[1] <- "Coefficient"
    coef_df <- coef_df[coef_df$Variable != "(Intercept)", ]
    coef_df <- coef_df[order(abs(coef_df$Coefficient), decreasing = TRUE), ]
  } else {
    coef_df <- coef_df[order(abs(coef_df$Coefficient), decreasing = TRUE), ]
  }

  top_vars <- head(coef_df$Variable, top_n)

  return(X_matrix[, top_vars, drop = FALSE])
}
