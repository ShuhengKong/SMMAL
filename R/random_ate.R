#' Title
#'
#' @param Y Numeric vector. Observed outcomes.
#' @param A Numeric vector. Treatment indicator (1 for treated, 0 for control).
#'
#' @return A numeric value representing the randomized ATE (SSL-based estimate).
#' @export
#'
random_ate <- function(Y, A, mu1, mu0, pi1, pi0, imp.A = NULL,
                        imp.A1Y1 = NULL, imp.A0Y1 = NULL,
                        min.pi = 0.05, max.pi = 0.95) {

  pi1 = pmin(max.pi, pmax(min.pi, pi1))
  pi1 = pi1 * mean(A) / mean(pi1)
  pi0 = pmin(max.pi, pmax(min.pi, pi0))
  pi0 = pi0 * mean(A) / mean(pi0)
  infl_sl = mu1 + A / pi1 * (Y - mu1) - mu0 - (1 - A) / pi0 * (Y - mu0)
  est_sl = mean(infl_sl)
  se_sl = sd(infl_sl) / sqrt(length(Y))


  est_ssl = NA
  se_ssl = NA
  if (!is.null(imp.A) && !is.null(imp.A1Y1) && !is.null(imp.A0Y1)) {
    n = length(Y)
    N = length(pi1)
    rho.inv = N / n

    infl_ssl = (
      (mu1 + imp.A1Y1 / pi1 - imp.A * mu1 / pi1) -
        (mu0 + imp.A0Y1 / pi0 - (1 - imp.A) * mu0 / pi0)
    )
    infl_ssl[1:n] = infl_ssl[1:n] + rho.inv * (
      (A * Y - imp.A1Y1[1:n]) / pi1[1:n]
      - ((1 - A) * Y - imp.A0Y1[1:n]) / pi0[1:n]
      - (A - imp.A[1:n]) * (mu1[1:n] / pi1[1:n] + mu0[1:n] / pi0[1:n])
    )

    est_ssl = mean(infl_ssl)
    se_ssl = sd(infl_ssl) / sqrt(N)
  }


  return(list(
    ate_sl = list(est = est_sl, se = se_sl),
    ate_ssl = list(est = est_ssl, se = se_ssl)
  ))

}



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
