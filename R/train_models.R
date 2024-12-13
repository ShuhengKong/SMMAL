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

  W = cbind(dat$X, dat$S)

  foldid = c(rep_len(1:nfold, length.out = n)[sample(n)],
             rep_len(1:nfold, length.out = N-n)[sample(N-n)])

  pi.bs = cf.bs(dat$A[1:n], dat$X, nfold, foldid,
                fit.fun=fit.fun, link = expit,
                dev.fun = dev.binary.np)
  # pi.bs$meas
  mu1.bs = cf.bs(dat$Y[1:n], dat$X, nfold, foldid,
                 subset = dat$A[1:n]==1,
                 fit.fun=fit.fun, link = expit,
                 dev.fun = dev.binary.np)
  # mu1.bs$meas
  mu0.bs = cf.bs(dat$Y[1:n], dat$X, nfold, foldid,
                 subset = dat$A[1:n]==0,
                 fit.fun=fit.fun, link = expit,
                 dev.fun = dev.binary.np)

  # SSL models
  imp.A.bs = cf.bs(dat$A[1:n], W, nfold, foldid)
  # imp.A.bs$meas
  # imp.A1Y1.bs = cf.bs(dat$A[1:n]*dat$Y[1:n], W, nfold, foldid)
  imp.A1Y1.bs = cf.bs(dat$Y[1:n], W, nfold, foldid,
                      subset = dat$A[1:n]==1,
                      fit.fun=fit.fun, link = expit,
                      dev.fun = dev.binary.np)
  # imp.A1Y1.bs$meas
  # imp.A0Y1.bs = cf.bs((1-dat$A[1:n])*dat$Y[1:n], W, nfold, foldid)
  imp.A0Y1.bs = cf.bs(dat$Y[1:n], W, nfold, foldid,
                      subset = dat$A[1:n]==0,
                      fit.fun=fit.fun, link = expit,
                      dev.fun = dev.binary.np)









  X_matrix_raw <-3
  Y_matrix_raw <-4

  return(list(X_matrix_raw=X_matrix_raw,Y_matrix_raw=Y_matrix_raw))
}
