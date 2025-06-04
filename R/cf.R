#' Cross-Fitting with Model Selection and Log Loss Evaluation
#'
#' Trains and evaluates predictive models using cross-fitting across \code{nfold} folds,
#' supporting multiple learner types. Outputs out-of-fold predictions and computes \code{log_loss}
#' for each hyperparameter tuning round to select the best-performing model.
#'
#' @param Y Numeric or factor vector. The response variable, either binary (0/1) or continuous.
#' Only labelled observations (where \code{R = 1}) are used.
#' @param X Matrix or data frame. Predictor variables used for model training.
#' @param nfold Integer. Number of cross-fitting folds.
#' @param R Binary vector. Indicator of labelled data: 1 = labelled, 0 = unlabelled.
#' @param foldid Integer vector. Fold assignments for cross-fitting (length equal to the full dataset).
#' @param cf_model Character string. Specifies the model type. Must be one of \code{"xgboost"}, \code{"bspline"}, or \code{"randomforest"}.
#' @param sub_set Logical vector. Indicates which labelled samples to include in training.
#' @param custom_model_fun A logical or function. If \code{NULL} or \code{FALSE}, bypasses adaptive-LASSO feature selection. Otherwise, enables two-stage tuning inside \code{compute_parameter()}.
#' Defaults to all \code{TRUE}.
#'
#' @return A list containing:
#' \describe{
#'   \item{models}{(Currently a placeholder) List of trained models per fold and tuning round.}
#'   \item{predictions}{List of out-of-fold predictions for each of the 5 tuning rounds.}
#'   \item{log_losses}{Numeric vector of log loss values for each tuning round.}
#'   \item{best_rounds_index}{Integer index (1–5) of the round achieving the lowest \code{log_loss}.}
#'   \item{best_rounds_log_losses}{Minimum \code{log_loss} value achieved across rounds.}
#'   \item{best_rounds_prediction}{Vector of out-of-fold predictions from the best tuning round.}
#' }
#'
#' @details
#' The function supports three learner types:
#' \itemize{
#'   \item{\strong{xgboost}}: Gradient-boosted trees, tuning \code{gamma} across rounds.
#'   \item{\strong{bspline}}: Logistic regression using B-spline basis expansions, tuning the number of knots.
#'   \item{\strong{randomforest}}: Random forests, tuning \code{nodesize}.
#' }
#'
#' Cross-fitting ensures that model evaluation is based on out-of-fold predictions,
#' reducing overfitting. \code{log_loss} is used as the evaluation metric to identify the
#' best hyperparameter setting.
#'
#' @examples
#' set.seed(123)
#' N <- 200
#' X <- matrix(rnorm(N * 5), nrow = N, ncol = 5)
#'
#' # Simulate treatment assignment
#' A <- rbinom(N, 1, plogis(X[, 1] - 0.5 * X[, 2]))
#'
#' # Simulate outcome
#' Y_full <- rbinom(N, 1, plogis(0.5 * X[, 1] - 0.25 * X[, 3]))
#'
#' # Introduce some missingness to simulate semi-supervised data
#' Y <- Y_full
#' Y[sample(1:N, size = N/4)] <- NA  # 25% missing
#'
#' # Create R vector (labelled = 1, unlabelled = 0)
#' R <- ifelse(!is.na(Y), 1, 0)
#'
#' # Cross-validation fold assignment
#' foldid <- sample(rep(1:5, length.out = N))
#'
#' # Run cf with bspline model
#' result <- cf(Y = Y, X = X, nfold = 5, R = R, foldid = foldid, cf_model = "bspline")
#'
#' # Examine output
#' print(result$log_losses)
#' print(result$best_rounds_index)
#'
#' @import xgboost
#' @import randomForest
#' @import splines2
#' @import glmnet
#' @importFrom stats glm binomial
#' @export


#function role:train and predict with selected model; compute evaluation metric(log loss)
cf <- function(Y, X, nfold, R,foldid,cf_model,sub_set = rep(TRUE, length(Y)), custom_model_fun = NULL) {
  N=length(Y)
  labeled_indices <- which(R == 1)
  Y <- Y[labeled_indices]

  foldid_labelled <- foldid[labeled_indices]
  sub_set <- sub_set[labeled_indices]

  if (is.data.frame(X)) {
    X <- as.matrix(X[labeled_indices, , drop = FALSE])
  } else {
    X <- as.matrix(X[labeled_indices])
  }

  results <- list()
  fold_predictions <- list()
  all_preds <- rep(NA, length(Y))

  log_loss <- function(y_true, y_pred) {
    epsilon <- 1e-15
    y_pred <- pmax(pmin(y_pred, 1 - epsilon), epsilon)
    return(-mean(y_true * log(y_pred) + (1 - y_true) * log(1 - y_pred)))
  }

if (!is.null(custom_model_fun)) {
    fold_predictions <- custom_model_fun(X,Y,foldid_labelled,sub_set,labeled_indices,nfold,log_loss)
}





  if (!is.null(cf_model) && cf_model == "glm" && is.null(custom_model_fun))
  {
    for (rounds_index in 1:5) {
      all_preds <- rep(NA, N)

      for (ifold in 1:nfold) {
        trainpos <- which((foldid_labelled != ifold) & sub_set)
        testpos  <- which(foldid_labelled == ifold)
        X_train <- as.matrix(X[trainpos, , drop = FALSE])
        Y_train <- as.numeric(Y[trainpos])
        X_test  <- as.matrix(X[testpos, , drop = FALSE])
        valid_idx <- which(!is.na(Y_train))
        X_train <- X_train[valid_idx, , drop = FALSE]
        Y_train <- Y_train[valid_idx]
        df_train <- data.frame(Y = Y_train, X = X_train[, 1])
        df_test  <- data.frame(X = X_test[, 1])

        model <- glm(Y ~ X, data = df_train, family = binomial())
        all_preds[testpos] <- predict(model, newdata = df_test, type = "response")
      }

      fold_predictions[[rounds_index]] <- all_preds
    }

  }

  if(!is.null(cf_model) && cf_model=="xgboost" && is.null(custom_model_fun))
  {
    for (rounds_index in 1:5) {
      fold_preds <- vector("list", nfold)
      for (ifold in 1:nfold) {
        trainpos <- which((foldid_labelled != ifold) & sub_set)
        testpos <- which(foldid == ifold)
        X_train <- as.matrix(X[trainpos])
        Y_train <- as.numeric(as.factor(Y[trainpos])) - 1  # Convert to binary 0/1
        X_test <- as.matrix(X[testpos])
        dtrain <- xgb.DMatrix(data = X_train, label = Y_train)

        params <- list(
          objective = "binary:logistic",
          eval_metric = "logloss",
          max_depth = 9,
          eta = 0.01,
          gamma = 0.25*(rounds_index-1),
          subsample = 1,
          min_child_weight = 1,
          colsample_bytree = 0.7
        )

        watchlist <- list(train = dtrain)

        model <- xgb.train(
          params = params,
          data = dtrain,
          nrounds = 2000,
          early_stopping_rounds = 10,
          watchlist = watchlist,
          verbose = 0
        )

        all_preds[testpos] <- predict(model, xgb.DMatrix(data = X_test))
        #all_preds[testpos] <- ifelse(predict(model, xgb.DMatrix(data = X_test))>0.5,1,0)
      }
      fold_predictions[[rounds_index]] <- all_preds
    }

  }

  if(!is.null(cf_model) && cf_model=="bspline" && is.null(custom_model_fun))
  {
    for (rounds_index in 1:5) {
      fold_preds <- vector("list", nfold)
      for (ifold in 1:nfold) {

        trainpos <- which((foldid_labelled != ifold) & sub_set[labeled_indices])
        testpos <- which(foldid_labelled == ifold)
        X_train <- as.matrix(X[trainpos])
        Y_train <- as.numeric(Y[trainpos])
        X_test <- as.matrix(X[testpos])

        # Remove rows with missing Y in training
        valid_idx <- which(!is.na(Y_train))
        X_train <- X_train[valid_idx, , drop = FALSE]
        Y_train <- Y_train[valid_idx]

        knots <- quantile(as.numeric(X_train), probs = seq(0.1, 0.9, length.out = 1+2*rounds_index))  # Adjust

        X_spline <- bSpline(x = as.numeric(c(X_train, X_test)), knots = knots, degree = 1, intercept = TRUE, eps = 1e-5)

        X_train_spline <- X_spline[1:nrow(X_train), , drop = FALSE]
        X_test_spline <- X_spline[(nrow(X_train) + 1):nrow(X_spline), , drop = FALSE]

        cv_model <- cv.glmnet(X_train_spline, Y_train, family = "binomial", alpha = 0)
        best_lambda <- cv_model$lambda.min

        model <- glmnet(X_train_spline, Y_train, family = "binomial", alpha = 0, lambda = best_lambda)

        all_preds[testpos] <- predict(model, X_test_spline, type = "response")[, 1]
        #all_preds[testpos] <- ifelse(predict(model, X_test_spline, type = "response")[, 1]>0.5,1,0)
      }
      fold_predictions[[rounds_index]] <- all_preds

    }

  }

  if(!is.null(cf_model) && cf_model=="randomforest" && is.null(custom_model_fun))
  {
    for (rounds_index in 1:5) {
      fold_preds <- vector("list", nfold)

      for (ifold in 1:nfold) {
        trainpos <- which((foldid_labelled != ifold) & sub_set)
        testpos <- which(foldid == ifold)
        X_train <- as.matrix(X[trainpos])
        Y_train <- as.factor(Y[trainpos])  # Ensure Y_train is factor for randomForest
        X_test <- as.matrix(X[testpos])

        model <- randomForest(x = X_train, y = Y_train, ntree = 1000,nodesize=1+2*rounds_index,replace = TRUE,mtry = sqrt(ncol(X_train)))

        # all_preds[testpos] <- as.numeric(predict(model, newdata = X_test, type = "response")) - 1

        all_preds[testpos] <- predict(model, newdata = X_test, type = "prob")[, 2]
      }

      fold_predictions[[rounds_index]] <- all_preds
    }

  }

  valid_preds <- Filter(Negate(is.null), fold_predictions)
  log_losses <- sapply(valid_preds, function(pred) log_loss(Y, pred))

  best_rounds_index <- which.min(log_losses)



  return(list(
    models = results,
    predictions = valid_preds,
    log_losses = log_losses,
    best_rounds_index = best_rounds_index,
    best_rounds_log_losses = log_losses[best_rounds_index],
    best_rounds_prediction = valid_preds[[best_rounds_index]]
  ))
}

