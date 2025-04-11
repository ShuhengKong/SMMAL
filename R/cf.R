


#' Cross-Fitting with Model Selection and Log Loss Evaluation
#'
#' Trains and evaluates a predictive model using cross-fitting across \code{K} folds,
#' supporting multiple learner types. Returns out-of-fold predictions and log loss for each
#' tuning round to select the best-performing model.
#'
#' @param Y Numeric or factor vector. Response variable. Can be binary (0/1) or continuous. Only labelled observations (R = 1) are used.
#' @param X Matrix or data frame. Covariates used for model training.
#' @param K Integer. Number of cross-fitting folds.
#' @param R Binary vector. 1 indicates labelled data; 0 indicates unlabelled. Used to filter training samples.
#' @param foldid Integer vector. Fold assignments for cross-fitting (length = full dataset).
#' @param cf_model Character string. Name of the model to use. One of \code{"xgboost"}, \code{"bspline"}, or \code{"randomforest"}.
#' @param subset Logical vector. Indicates which samples (among labelled) to include in training. Defaults to all \code{TRUE}.
#'
#' @return A list with:
#' \describe{
#'   \item{models}{(Currently placeholder) Trained models per fold and tuning round.}
#'   \item{predictions}{A list of length 5 (one per tuning round) of out-of-fold predictions.}
#'   \item{log_losses}{Vector of log loss values for each tuning round.}
#'   \item{Tuning_Parameter}{The hyperparameter varied across the 5 rounds (e.g., gamma, number of knots, nodesize).}
#'   \item{best_rounds_index}{Index (1-5) of the round with the lowest log loss.}
#'   \item{best_rounds_log_losses}{Minimum log loss achieved.}
#'   \item{best_rounds_prediction}{Vector of out-of-fold predictions from the best tuning round.}
#' }
#'
#' @details
#' The function supports three types of models:
#' \itemize{
#'   \item{\strong{xgboost}}: Trains gradient-boosted trees with varying \code{gamma} across rounds.
#'   \item{\strong{bspline}}: Trains logistic regression with B-spline basis features and different knot numbers.
#'   \item{\strong{randomforest}}: Trains random forests with varying \code{nodesize}.
#' }
#' At each round, cross-fitting is applied to prevent overfitting. Out-of-fold predictions are collected,
#' and log loss is computed to identify the best hyperparameter setting.
#'
#' @import xgboost
#' @import randomForest
#' @import splines2
#' @import glmnet
#' @export



#function role:train and predict with selected model; compute evaluation metric(log loss)
cf <- function(Y, X, K, R,foldid,cf_model,subset = rep(TRUE, length(Y))) {
  labeled_indices <- which(R == 1)
  Y <- Y[labeled_indices]

  foldid_labelled <- numeric(length(Y))
  foldid_labelled[R == 1] <- foldid[R == 1]

  if (is.data.frame(X)) {
    X <- as.matrix(X[labeled_indices, , drop = FALSE])
  } else {
    X <- as.matrix(X[labeled_indices])
  }

  results <- list()
  fold_predictions <- list()
  all_preds <- rep(NA, length(Y))


  #Three model were selected
  # xgboost
  # bspline
  # randomforest


  if(cf_model=="xgboost")
  {
    for (rounds_index in 1:5) {
      fold_preds <- vector("list", K)
      for (ifold in 1:K) {
        trainpos <- which((foldid_labelled != ifold) & subset)
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

    Tuning_Parameter="gamma"
  }

  if(cf_model=="bspline")
  {
    for (rounds_index in 1:5) {
      fold_preds <- vector("list", K)
      for (ifold in 1:K) {
        trainpos <- which((foldid_labelled != ifold) & subset[labeled_indices])
        testpos <- which(foldid_labelled == ifold)
        X_train <- as.matrix(X[trainpos])
        Y_train <- as.numeric(Y[trainpos])
        X_test <- as.matrix(X[testpos])

        #一起生成
        #加完全一样的data
        # X_train_spline 和 X_test_spline是否一样

        knots <- quantile(as.numeric(X_train), probs = seq(0.1, 0.9, length.out = 1+2*rounds_index))  # Adjust

        X_spline <- bSpline(x = as.numeric(c(X_train, X_test)), knots = knots, degree = 1, intercept = TRUE, eps = 1e-5)

        X_train_spline <- X_spline[1:length(X_train), , drop = FALSE]
        X_test_spline <- X_spline[(length(X_train) + 1):nrow(X_spline), , drop = FALSE]

        cv_model <- cv.glmnet(X_train_spline, Y_train, family = "binomial", alpha = 0)
        best_lambda <- cv_model$lambda.min

        model <- glmnet(X_train_spline, Y_train, family = "binomial", alpha = 0, lambda = best_lambda)

        all_preds[testpos] <- predict(model, X_test_spline, type = "response")[, 1]
        #all_preds[testpos] <- ifelse(predict(model, X_test_spline, type = "response")[, 1]>0.5,1,0)
      }
      fold_predictions[[rounds_index]] <- all_preds

    }

    Tuning_Parameter="(number of) knots"
  }

  if(cf_model=="randomforest")
  {
    for (rounds_index in 1:5) {
      fold_preds <- vector("list", K)

      for (ifold in 1:K) {
        trainpos <- which((foldid_labelled != ifold) & subset)
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

    Tuning_Parameter="nodesize"
  }



  log_loss <- function(y_true, y_pred) {
    epsilon <- 1e-15
    y_pred <- pmax(pmin(y_pred, 1 - epsilon), epsilon)
    return(-mean(y_true * log(y_pred) + (1 - y_true) * log(1 - y_pred)))
  }

  mse <- function(y_true, y_pred) {
    return(mean((y_true - y_pred)^2))
  }

  valid_preds <- Filter(Negate(is.null), fold_predictions)
  log_losses <- sapply(valid_preds, function(pred) log_loss(Y, pred))
  mse_values <- sapply(valid_preds, function(pred) mse(Y, pred))


  best_rounds_index <- which.min(log_losses)



  return(list(
    models = results,
    predictions = valid_preds,
    log_losses = log_losses,
    Tuning_Parameter=Tuning_Parameter,
    best_rounds_index = best_rounds_index,
    best_rounds_log_losses = log_losses[best_rounds_index],
    best_rounds_prediction = valid_preds[[best_rounds_index]]
  ))
}
