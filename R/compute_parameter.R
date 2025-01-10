#' Title
#'
#' @param K Integer. The number of folds for cross-validation.
#' @param folds Integer vector. Fold assignments for each observation.
#' @param X Matrix. Predictor matrix (\eqn{n \times p}) used for model training.
#' @param S Matrix. Auxiliary predictor matrix (\eqn{n \times q}) used for training.
#' @param X_matrix Matrix. Refined predictor matrix (\eqn{n \times p}) for storing intermediate results.
#' @param Y_matrix Matrix. Refined response matrix (\eqn{n \times m}) for storing intermediate results.
#' @param parameter_list_W List. A list of parameters associated with the combined design matrix \eqn{W}.
#' @param parameter_list_Y List. A list of parameters associated with the response matrix \eqn{Y}.
#'
#' @return A list containing:
#' \describe{
#'   \item{pi_1}{Numeric. Computed propensity score for the treatment group (\eqn{\pi_1}).}
#'   \item{pi_0}{Numeric. Computed propensity score for the control group (\eqn{\pi_0}).}
#'   \item{mu_a}{Numeric. Predicted outcome under treatment.}
#'   \item{Pi_a}{Numeric. Predicted propensity score from auxiliary models.}
#'   \item{m_a}{Numeric. Auxiliary outcome predictions.}
#' }
#' @export
#'
compute_parameter <- function(K,folds,X,S,X_matrix,Y_matrix,parameter_list_W,parameter_list_Y){



  mu1.bs = cf.xgboost(dat$Y[1:n], dat$X, nfold, foldid,
                 subset = dat$A[1:n]==1,
                 fit.fun=fit.fun, link = expit,
                 dev.fun = dev.binary.np)

  mu0.bs = cf.xgboost(dat$Y[1:n], dat$X, nfold, foldid,
                 subset = dat$A[1:n]==0,
                 fit.fun=fit.fun, link = expit,
                 dev.fun = dev.binary.np)



  pi1.bs = cf.xgboost(dat$A[1:n], dat$X, nfold, foldid,
                fit.fun=fit.fun, link = expit,
                dev.fun = dev.binary.np)


  # cap_pi1
  imp.A.bs = cf.xgboost(dat$A[1:n], W, nfold, foldid)
  # m1
  imp.A1Y1.bs = cf.xgboost(dat$Y[1:n], W, nfold, foldid,
                      subset = dat$A[1:n]==1,
                      fit.fun=fit.fun, link = expit,
                      dev.fun = dev.binary.np)
  # m0
  imp.A0Y1.bs = cf.xgboost(dat$Y[1:n], W, nfold, foldid,
                      subset = dat$A[1:n]==0,
                      fit.fun=fit.fun, link = expit,
                      dev.fun = dev.binary.np)


  return(list(pi1=pi1,pi0=1-pi0,mu1=mu1,mu0=mu0,cap_pi1=imp.A.bs,cap_pi0=1-imp.A.bs, m1=imp.A1Y1.bs, m0=imp.A0Y1.bs ))
}





cf.rf = function(Y, X, nfold, foldid, subset = rep(T,length(Y)),
                 dev.fun = ifelse(all(Y %in% 0:1), dev.binary.np, dev.ls),
                 ...)
{

  subset = dat$A[1:n]==1
  p.range = 3:round(sqrt(length(Y)))
  fit.fun = function(formula,...){coef(lm(formula))}
  dev.fun = dev.ls
  degree = 1

  # body
  n = length(Y)
  N = nrow(X)
  sub.pos = which(subset)
  if(all(class(X)!="data.frame"))
  {
    X = data.frame(X)
  }

  options(warn = -1)


  ## bagging, random select a x from sub.pos and y from subset
  full.fit = randomForest(x=X[sub.pos,,drop=F], y=Y[subset], xtest = X)
  options(warn = 1)
  full.pred = full.fit$test$predicted
  cf.pred = rep(NA,N)
  for (ifold in 1:nfold)
  {
    trainpos = which(foldid[1:n]!=ifold & subset)
    testpos = which(foldid[1:n]==ifold & subset)
    options(warn = -1)
    # print(length(Y[trainpos]))
    # print(length(X[trainpos,]))
    cf.fit = randomForest(x=X[trainpos,,drop=F], y=Y[trainpos],
                          xtest = X[testpos,,drop=F],
                          ytest = Y[testpos],
                          keep.forest = T)
    options(warn = 1)

    predpos = which(foldid==ifold)
    cf.pred[predpos] = predict(cf.fit, X[predpos,,drop=F])
  }
  dev.rf = dev.fun(Y[subset], cf.pred[sub.pos])

  if(all(Y %in% 0:1))
  {
    meas = c(auc = auc(Y[subset], cf.pred[sub.pos],
                       levels = c(0,1), direction = "<"))
  }else{
    meas = c(Rsq = 1 - mean((Y[subset] - cf.pred[sub.pos])^2)/var(Y[subset]))
  }

  return(list(cf.pred = cf.pred, full.pred = full.pred,
              dev = dev.rf,
              meas = meas))
}



cf.xgboost = function(Y, X, n, nfold, foldid, subset = rep(T,length(Y)), param)
{
  Y = dat$Y[1:n]
  X = dat$X[1:n]  #需要Y与X同长度
  subset = dat$A[1:n]==1

  if (is.null(params)) {
  results = list()
  fold_predictions = list()
  for (ifold in 1:nfold)
  {

    trainpos = which(foldid[1:n]!=ifold & subset)
    testpos = which(foldid[1:n]==ifold & subset)
    options(warn = -1)

   # Y_noisy = Y + rnorm(length(Y), mean = 0, sd = 0.2)
    combine_data=data.frame(X, Y = Y)

    train_control = trainControl(method = "cv", number = nfold, search = "grid")
    xgbGrid <-  expand.grid(max_depth = c(3, 5, 7),
                            nrounds = (1:10)*50,
                            eta = 0.3,
                            gamma = 0,
                            subsample = 1,
                            min_child_weight = 1,
                            colsample_bytree = 0.6)


    xgbGridSearch = train(Y~., data = data.frame(combine_data[trainpos,]), method = "xgbTree", trControl = train_control, tuneGrid = xgbGrid)


    results[[ifold]] = xgbGridSearch
    fold_predictions[[ifold]] = predict(xgbGridSearch, newdata = combine_data[testpos, ])

  }


  best_fold = which.min(sapply(results, function(res) min(res$results$logLoss)))
  predictions = predict(results[[best_fold]], newdata = combine_data)


  }
  else{
    params = list(
      objective = "binary:logistic",
      eval_metric = "logloss",
      max_depth = 5,
      eta = 0.1,
      subsample = 0.6,
      colsample_bytree = 0.6
    )

    dtrain <- xgb.DMatrix(data = X[subset, , drop = FALSE], label = Y[subset])
    final_model <- xgboost(
      params = params,
      data = dtrain,
      nrounds = nfold
    )

    predictions <- predict(final_model, xgb.DMatrix(data = X))

  }


  return(list(predictions = predictions))
  #probability
  # reg:squarederror   regression mse

}
