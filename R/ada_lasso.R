#' Variable Selection Using Adaptive LASSO
#'
#' Performs variable selection using the adaptive LASSO (Least Absolute Shrinkage and Selection Operator)
#' via cross-validated logistic regression with \code{glmnet}. The function selects important predictors
#' based on the nonzero coefficients from the adaptive LASSO fit.
#'
#' If only a single predictor is provided, a univariate logistic regression is performed using \code{glm}.
#' The predictor is selected if its p-value is below 0.05.
#'
#' @param X A data frame or matrix of predictor variables.
#' @param Y A response vector (binary outcome: 0 or 1), possibly containing missing values.
#'
#' @return A matrix containing the selected columns of \code{X} corresponding to important predictors.
#' If no variables are selected, the function returns \code{NULL}.
#'
#' @import glmnet
#' @importFrom stats glm binomial
#'
#' @examples
#' set.seed(123)
#' n <- 100
#' p <- 10
#'
#' # Generate predictors
#' X <- matrix(rnorm(n * p), nrow = n, ncol = p)
#'
#' # True model: only variables 1 and 3 affect Y
#' beta <- c(1.5, 0, -2, rep(0, p - 3))
#' logits <- X %*% beta
#' probs <- 1 / (1 + exp(-logits))
#'
#' # Generate binary outcomes
#' Y <- rbinom(n, size = 1, prob = probs)
#'
#' # Apply ada_lasso
#' selected_X <- ada_lasso(X, Y)
#'
#' if (!is.null(selected_X)) {
#'   print(dim(selected_X))  # Show dimensions of selected variables
#' } else {
#'   print("No variables selected.")
#' }
#'
#' @export

ada_lasso <- function(X, Y) {
  observed_idx <- which(!is.na(Y))
  X_obs <- X[observed_idx, , drop = FALSE]
  Y_obs <- Y[observed_idx]

  #glm for vector X
  if (ncol(X_obs) == 1) {
    # Use glm to assess univariate predictor
    df <- data.frame(x = X_obs[, 1], y = Y_obs)
    fit <- glm(y ~ x, data = df, family = binomial())
    pval <- summary(fit)$coefficients[2, 4]

    # Keep if significant at 0.05 level
    if (pval < 0.05) {
      return(X[, , drop = FALSE])
    } else {
      return(NULL)
    }
  }

  X_matrix <- as.matrix(X_obs)
  Y_vector <- as.numeric(Y_obs)

  cv_fit1 <- cv.glmnet(X_matrix, Y_vector, alpha = 1, standardize = TRUE)
  beta_init <- as.vector(coef(cv_fit1, s = "lambda.min"))[-1]  # exclude intercept

  weights <- 1 / (abs(beta_init) + 1e-4)

  cv_fit2 <- cv.glmnet(X_matrix, Y_vector, alpha = 1, penalty.factor = weights, standardize = TRUE)
  beta_final <- as.vector(coef(cv_fit2, s = "lambda.min"))[-1]

  top_vars <- which(beta_final != 0)

  if (length(top_vars) == 0) {
    return(NULL)
  }

  return(X[, top_vars, drop = FALSE])
}

