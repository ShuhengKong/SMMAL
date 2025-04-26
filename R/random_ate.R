#' Estimate Average Treatment Effect (ATE) via Semi-Supervised Learning
#'
#' @param Y Numeric vector. Observed outcomes for labelled data.
#' @param A Numeric vector. Treatment indicator (1 for treated, 0 for control).
#' @param mu1 Numeric vector. Estimated outcome regression \eqn{E[Y \mid A = 1, X]}.
#' @param mu0 Numeric vector. Estimated outcome regression \eqn{E[Y \mid A = 0, X]}.
#' @param pi1 Numeric vector. Estimated propensity scores \eqn{P(A = 1 \mid X)}.
#' @param pi0 Numeric vector. Estimated propensity scores \eqn{P(A = 0 \mid X)}.
#' @param imp.A Numeric vector. Estimated imputed propensity scores using surrogate covariates \code{W}.
#' @param imp.A1Y1 Numeric vector. Nuisance model prediction for \eqn{E[Y \mid A = 1, W]}.
#' @param imp.A0Y1 Numeric vector. Nuisance model prediction for \eqn{E[Y \mid A = 0, W]}.
#' @param min.pi Numeric. Minimum value for propensity scores to avoid division by zero (default = 0.05).
#' @param max.pi Numeric. Maximum value for propensity scores to avoid division by zero (default = 0.95).
#'
#' @return A list containing:
#' \describe{
#'   \item{est}{Estimated ATE.}
#'   \item{se}{Standard error of the ATE estimator.}
#' }
#'
#' @details
#' This estimator adjusts for missing outcomes using a semi-supervised learning framework,
#' leveraging both labelled and unlabelled information through surrogate variables.
#'
#' @examples
#' set.seed(123)
#' N <- 400
#' n <- 200  # Number of labelled observations
#'
#' # Generate covariates and treatment
#' X <- rnorm(N)
#' A <- rbinom(N, 1, plogis(X))
#'
#' # True potential outcomes
#' Y0_true <- X + rnorm(N)
#' Y1_true <- X + 1 + rnorm(N)
#'
#' # Observed outcomes
#' Y_full <- ifelse(A == 1, Y1_true, Y0_true)
#'
#' # Only first n samples are labelled
#' Y <- rep(NA, N)
#' Y[1:n] <- Y_full[1:n]
#'
#' # Nuisance parameter estimates
#' mu1 <- X + 0.5
#' mu0 <- X - 0.5
#' pi1 <- plogis(X)
#' pi0 <- 1 - pi1
#' imp.A <- plogis(X)
#' imp.A1Y1 <- plogis(X) * (X + 0.5)
#' imp.A0Y1 <- (1 - plogis(X)) * (X - 0.5)
#'
#' # Estimate ATE
#' result <- ate.SSL(
#'   Y = Y[1:n],
#'   A = A[1:n],
#'   mu1 = mu1[1:n],
#'   mu0 = mu0[1:n],
#'   pi1 = pi1[1:n],
#'   pi0 = pi0[1:n],
#'   imp.A = imp.A,
#'   imp.A1Y1 = imp.A1Y1,
#'   imp.A0Y1 = imp.A0Y1
#' )
#'
#' print(result$est)
#' print(result$se)
#'
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
