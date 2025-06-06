#' Adaptive LASSO with Cross-Validation
#'
#' Performs adaptive LASSO for binary outcomes by first fitting a ridge regression to compute
#' penalty factors, and then running cross-validated lasso fits over a grid of lambda values.
#'
#' @param X A numeric matrix of predictors (n observations × p features).
#' @param Y A numeric or integer vector of binary outcomes (length n).
#' @param foldid_labelled An integer vector (length n) of fold assignments for labeled observations.
#'        Values should run from 1 to \code{nfold}; other values (e.g., \code{NA}) indicate unlabeled or held-out rows.
#' @param sub_set A logical or integer vector indicating which rows of \code{X}/\code{Y} are used in supervised CV.
#' @param labeled_indices An integer or logical vector indicating which rows have non-missing outcomes.
#' @param nfold A single integer specifying the number of CV folds (e.g., 5 or 10).
#' @param log_loss A function of the form \code{function(true_labels, pred_probs)} that returns a single log-loss numeric.
#'
#' @return A \code{list} of length equal to the number of ridge penalty values provided by \code{param_fun()}.
#'         Each element is a numeric vector (length = n) containing cross-validated predicted probabilities
#'         for the best \code{lambda} under that ridge penalty.
#'
#' @details
#' This function expects that a parameter-generating function \code{param_fun()} is available in the package,
#' returning a list with elements \code{$ridge} (a vector of ridge penalty values) and \code{$lambda} (a vector
#' of lasso penalty values).  Internally, it:
#' \enumerate{
#'   \item Fits a ridge-penalized logistic regression on all data to obtain coefficients.
#'   \item Computes penalty factors as \code{1 / (abs(coef) + 1e-4)}.
#'   \item For each ridge value, runs n-fold CV over \code{lambda} values with \code{glmnet(..., alpha=1)}.
#'   \item Records predictions on held-out folds, computes log-loss for each \code{lambda}, and selects the
#'         \code{lambda} with minimum log-loss.
#'   \item Returns a list of CV-predicted probability vectors (one vector per ridge value).
#' }
#'
#' @examples
#' \donttest{
#' # Assume param_fun() is defined elsewhere and returns:
#' #   list(ridge = c(0.01, 0.1, 1), lambda = exp(seq(log(0.001), log(1), length = 50)))
#'
#' # Simulate small data:
#' set.seed(123)
#' n <- 100; p <- 10
#' X <- matrix(rnorm(n * p), nrow = n)
#' true_beta <- c(rep(1.5, 3), rep(0, p - 3))
#' lin <- X %*% true_beta
#' probs <- 1 / (1 + exp(-lin))
#' Y <- rbinom(n, 1, probs)
#'
#' # Create fold assignments for labeled observations:
#' labeled <- sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.8, 0.2))
#' foldid_labelled <- rep(NA_integer_, n)
#' foldid_labelled[labeled] <- sample(1:5, sum(labeled), replace = TRUE)
#' sub_set         <- labeled
#' labeled_indices <- which(labeled)
#'
#' # Define a simple log-loss function:
#' log_loss_fn <- function(true, pred) {
#'   eps <- 1e-15
#'   pred_clipped <- pmin(pmax(pred, eps), 1 - eps)
#'   -mean(true * log(pred_clipped) + (1 - true) * log(1 - pred_clipped))
#' }
#'
#' # Call ada_lasso (wrapped to avoid lines > 100 characters):
#' results <- ada_lasso(
#'   X,
#'   Y,
#'   foldid_labelled   = foldid_labelled,
#'   sub_set           = sub_set,
#'   labeled_indices   = labeled_indices,
#'   nfold             = 5,
#'   log_loss          = log_loss_fn
#' )
#'
#' # 'results' is a list (one element per ridge value), each a numeric vector of CV predictions.
#' }
#'
#' @export
ada_lasso <- function(
    X,
    Y,
    foldid_labelled,
    sub_set,
    labeled_indices,
    nfold,
    log_loss
) {

    fold_predictions <- NULL
    param_grid <- param_fun()
    ridge_list  <- param_grid$ridge
    lambda_list <- param_grid$lambda

    fold_predictions <- vector("list", length(ridge_list))

    for (r in seq_along(ridge_list)) {
      ridge_val <- ridge_list[[r]]

      ridge_fit_all <- glmnet::glmnet(
        X,
        Y,
        lambda = ridge_val,
        alpha = 0,
        family = "binomial"
      )
      ridge_coef <- as.numeric(coef(ridge_fit_all))[-1]
      penalty_factors <- 1 / (abs(ridge_coef) + 1e-4)

      all_preds_matrix <- matrix(NA, nrow = length(Y), ncol = length(lambda_list))

      for (ifold in seq_len(nfold)) {
        trainpos <- which((foldid_labelled != ifold) & sub_set[labeled_indices])
        testpos  <- which(foldid_labelled == ifold)

        X_train <- as.matrix(X[trainpos, , drop = FALSE])
        Y_train <- as.numeric(Y[trainpos])
        X_test  <- as.matrix(X[testpos, , drop = FALSE])

        valid_idx <- which(!is.na(Y_train))
        X_train <- X_train[valid_idx, , drop = FALSE]
        Y_train <- Y_train[valid_idx]

        fit <- glmnet::glmnet(
          X_train,
          Y_train,
          lambda         = lambda_list,
          alpha          = 1,
          family         = "binomial",
          penalty.factor = penalty_factors
        )

        preds <- predict(fit, newx = X_test, type = "response")
        all_preds_matrix[testpos, ] <- preds
      }

      fold_predictions[[r]] <- all_preds_matrix
    }

    inter_valid_preds <- Filter(Negate(is.null), fold_predictions)
    inter_logloss <- lapply(inter_valid_preds, function(pred_matrix) {
      valid_rows <- !is.na(pred_matrix[, 1])

      losses <- apply(pred_matrix, 2, function(pred_col) {
        log_loss(Y[valid_rows], pred_col[valid_rows])
      })

      best_col_index <- which.min(losses)
      matrix(pred_matrix[, best_col_index], ncol = 1)
    })

    fold_predictions <- inter_logloss

  return(fold_predictions)
}




