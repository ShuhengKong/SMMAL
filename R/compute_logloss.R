


#' Compare Cross-Fitting Model Log Losses Across Learners
#'
#' Runs the \code{cf} function using three different learning algorithms (\code{xgboost}, \code{bspline}, and \code{randomforest}),
#' and compiles the resulting log loss metrics across tuning rounds into a unified summary.
#'
#' @param K Integer. Number of cross-fitting folds.
#' @param Y Numeric or factor vector. Outcome variable (binary); NAs allowed for unlabelled data.
#' @param A Numeric or binary vector. Treatment indicator; not used in this function but passed for consistency.
#' @param X Matrix or data frame. Covariates used by the models.
#' @param S Matrix or data frame. Additional features (not used here but passed for interface consistency).
#' @param W Matrix or data frame. Combination of X and S (not used in this function).
#' @param foldid Integer vector. Fold assignments for cross-fitting.
#' @param R Binary vector. 1 for labelled samples (used to filter training data), 0 for unlabelled.
#'
#' @return A list containing:
#' \describe{
#'   \item{logloss_combined}{A data frame with columns: \code{Model}, \code{Tuning_Parameter}, and \code{LogLoss}. One row per tuning round for each model.}
#'   \item{logloss_xgboost_log_losses}{Vector of log loss values for each tuning round from xgboost.}
#'   \item{logloss_bspline_log_losses}{Vector of log loss values for each tuning round from bspline.}
#'   \item{logloss_randomforest_log_losses}{Vector of log loss values for each tuning round from randomforest.}
#'   \item{all_tuning_params}{Named list of tuning parameter names for each model.}
#' }
#'
#' @details
#' This function is helpful for comparing the cross-validated performance (in terms of log loss) of different predictive models
#' under a semi-supervised setting. Internally, it calls the \code{cf} function for each model type and aggregates the results.
#'
#' @export






#function role:polish the output of function cf
compute_logloss <- function(K, Y, A, X, S, W, foldid, R){

  logloss_xgboost = cf(Y, X, K, R, foldid, "xgboost")
  logloss_bspline = cf(Y, X, K, R, foldid, "bspline")
  logloss_randomforest = cf(Y, X, K, R, foldid, "randomforest")

  #create a table contain log_losses from all three models
  logloss_combined <- rbind(
    data.frame(Model = "xgboost", Tuning_Parameter = logloss_xgboost$Tuning_Parameter, LogLoss = logloss_xgboost$log_losses),
    data.frame(Model = "bspline", Tuning_Parameter = logloss_bspline$Tuning_Parameter, LogLoss = logloss_bspline$log_losses),
    data.frame(Model = "randomforest", Tuning_Parameter = logloss_randomforest$Tuning_Parameter, LogLoss = logloss_randomforest$log_losses)
  )

  all_tuning_params <- list(
    xgboost = logloss_xgboost$Tuning_Parameter,
    bspline = logloss_bspline$Tuning_Parameter,
    randomforest = logloss_randomforest$Tuning_Parameter
  )

  return(list(
    logloss_combined = logloss_combined,
    logloss_xgboost_log_losses = logloss_xgboost$log_losses,
    logloss_bspline_log_losses = logloss_bspline$log_losses,
    logloss_randomforest_log_losses = logloss_randomforest$log_losses,
    all_tuning_params = all_tuning_params
  ))
}
