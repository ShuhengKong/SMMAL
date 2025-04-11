#' Estimate Average Treatment Effect (ATE) via SL
#'
#' @param Y Numeric vector. Observed outcomes for labelled data.
#' @param A Numeric vector. Treatment indicator (1 for treated, 0 for control).
#' @param mu1 Numeric vector. Estimated \eqn{E[Y | A = 1, X]} for each individual.
#' @param mu0 Numeric vector. Estimated \eqn{E[Y | A = 0, X]} for each individual.
#' @param pi1 Numeric vector. Estimated propensity scores \eqn{P(A = 1 | X)}.
#' @param pi0 Numeric vector. Estimated propensity scores \eqn{P(A = 0 | X)}.
#' @param min.pi Numeric. Lower bound to truncate estimated propensity scores (default = 0.05).
#' @param max.pi Numeric. Upper bound to truncate estimated propensity scores (default = 0.95).
#'
#' @return A list with:
#' \describe{
#'   \item{est}{Estimated ATE.}
#'   \item{se}{Standard error of the ATE estimator.}
#' }
#'
#' @details
#' This estimator uses the efficient influence function for ATE under fully supervised learning,
#' incorporating both outcome regression and inverse probability weighting.
#'
#' @export

ate.SL = function(Y, A, mu1, mu0, pi1,pi0, min.pi = 0.05, max.pi = 0.95)
{
  pi1 = pmin(max.pi, pmax(min.pi, pi1))
  pi1 = pi1 * mean(A)/mean(pi1)
  pi0 = pmin(max.pi, pmax(min.pi, pi0))
  pi0 = pi0 * mean(A)/mean(pi0)

  infl = mu1 + A/pi1*(Y-mu1) - mu0 - (1-A)/pi0*(Y-mu0)
  return(list(est = mean(infl),
              se = sd(infl)/sqrt(length(Y))))
}

#' Estimate ATE using SSL
#'
#' @param Y Outcome vector
#' @param A Treatment assignment (0/1)
#' @param mu1 Estimated outcome under treatment
#' @param mu0 Estimated outcome under control
#' @param pi1 Estimated propensity score for A = 1
#' @param pi0 Estimated propensity score for A = 0
#' @param imp.A Estimated propensity score using covariates W
#' @param imp.A1Y1,imp.A0Y1 Nuisance model predictions
#' @param min.pi Minimum value for pi to avoid division by zero
#' @param max.pi Maximum value for pi to avoid division by zero
#'
#' @return A list with ATE estimate and standard error
#' @export
ate.SSL = function(Y,A, mu1, mu0, pi1,pi0, imp.A,
                   imp.A1Y1, imp.A0Y1, min.pi = 0.05, max.pi = 0.95)
{
  n = length(Y)
  N = length(pi1)
  rho.inv = N/n

  pi1 = pmin(max.pi, pmax(min.pi, pi1))
  pi1 = pi1 * mean(A)/mean(pi1)
  pi0 = pmin(max.pi, pmax(min.pi, pi0))
  pi0 = pi0 * mean(A)/mean(pi0)

  infl = (
    (mu1 + imp.A1Y1/pi1 - imp.A*mu1/pi1) -
      (mu0 + imp.A0Y1/pi0 - (1-imp.A)*mu0/pi0)
  )
  infl[1:n] = infl[1:n] + rho.inv*(
    (A*Y - imp.A1Y1[1:n])/pi1[1:n]
    -((1-A)*Y - imp.A0Y1[1:n])/pi0[1:n]
    - (A - imp.A[1:n])*(mu1[1:n]/pi1[1:n] + mu0[1:n]/pi0[1:n])
  )

  return(list(est = mean(infl),
              se = sd(infl)/sqrt(N)))
}
