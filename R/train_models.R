#' Title
#'
#' @param K Integer. The number of folds for cross-validation.
#' @param folds Integer vector. Fold assignments for each observation, with values from 1 to \eqn{K}.
#' @param X Matrix. Predictor matrix (\eqn{n \times p}), where \eqn{n} is the number of observations
#'   and \eqn{p} is the number of predictors.
#' @param A Numeric vector. Treatment or intervention indicator (binary or categorical).
#'
#' @return A list containing:
#' \describe{
#'   \item{X_matrix_raw}{Raw predictor matrix (\eqn{n \times p}) generated during model training.}
#'   \item{Y_matrix_raw}{Raw outcome matrix (\eqn{n \times m}) generated during model training.}
#' }
#' @export
#'
train_models <- function(K,folds,X,A){

  X_matrix_raw <-3
  Y_matrix_raw <-4

  return(list(X_matrix_raw=X_matrix_raw,Y_matrix_raw=Y_matrix_raw))
}
