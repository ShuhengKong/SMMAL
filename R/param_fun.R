#' Parameter grid function
#'
#' Returns two list of hyperparameters for model tuning.
#' @return A list, e.g., list(ridge = ..., lambda = ...)
#' @export
param_fun <- function() {
  ridge_values <- exp(seq(log(0.001), log(1), length.out = 5))
  lambda_values <- exp(seq(log(0.001), log(1), length.out = 20))
  list(
    ridge = ridge_values,
    lambda = lambda_values
  )
}
