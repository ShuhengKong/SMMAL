#' Fit Logistic Regression and Extract Coefficients
#'
#' This function fits a logistic regression model to the given data using the specified formula
#' and returns the model's coefficients. It serves as a simplified wrapper for `glm` with a binomial family.
#'
#' @param formula An object of class `formula` (e.g., `response ~ predictor1 + predictor2`) describing the model to be fitted.
#' @param data  Data
#' @param methos Which model to use
#' @param ... Additional arguments to be passed to `glm`, such as `data`, `weights`, or `subset`.
#'
#' @return A numeric vector containing the coefficients of the fitted logistic regression model.
#' @export
#'
fit.fun <- function(formula,data,method, ...) {
  if (method == "glm" || method == "logistic"){
    return(coef(glm(formula,data=data, family = binomial, ...)))
  }
  else if (method == "lm") {
    return(coef(lm(formula, data = data, ...)))
  }
  else if (method == "custom") {
    return(coef(lm(formula, data = data, ...)))
  }
}
