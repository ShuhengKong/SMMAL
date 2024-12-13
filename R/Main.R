#' Title
#'
#' @param Y Numeric vector. Observed outcomes.
#' @param A Numeric vector. Treatment indicator (binary or categorical).
#' @param S Matrix. Auxiliary predictor matrix (\eqn{n \times q}).
#' @param X Matrix. Predictor matrix (\eqn{n \times p}).
#' @param K Integer. The number of folds for cross-validation.
#' @param folds Integer vector. Fold assignments for each observation, with values from 1 to \eqn{K}.
#'
#' @return Numeric. The estimated Average Treatment Effect (ATE) using SSL methods.
#' @export
#'
main_function <- function(Y,A,S,X,K,folds){
  # Step 1
  result <- cross_validation(num_labelled, K, sample_data, num_x, num_S)
  Y <- result$Y
  A <- result$A
  X <- result$X
  S <- result$S
  W <- result$W
  foldid <- result$foldid



  # Step 2
  result_matrix_raw <- train_models(K,folds,X,A)
  X_matrix_raw <- result_matrix_raw$X_matrix_raw
  Y_matrix_raw <- result_matrix_raw$Y_matrix_raw

  # Step 3:
  result_matrix <- refine_models(K,folds, X,A,Y,X_matrix_raw,Y_matrix_raw)
  X_matrix <- result_matrix$X_matrix
  Y_matrix <- result_matrix$Y_matrix

  # Step 4:
  result_parameter<-compute_parameter(K,folds,X,S,X_matrix,Y_matrix,parameter_list_W,parameter_list_Y)

  pi_1 <- result_parameter$pi_1
  pi_0 <- result_parameter$pi_0
  mu_a <- result_parameter$mu_a
  Pi_a <- result_parameter$Pi_a
  m_a <- result_parameter$m_a

  ate_result_SSL <- random_ate(Y, A, mu_a, m_a, pi_1, pi_0, Pi_a)

  #return
  return(ate_result_SSL)
}
