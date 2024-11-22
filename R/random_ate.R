#' Title
#'
#' @param Y Numeric vector. Observed outcomes.
#' @param A Numeric vector. Treatment indicator (1 for treated, 0 for control).
#' @param mu_a Numeric vector. Predicted outcomes under treatment.
#' @param m_a Numeric vector. Predicted outcomes from auxiliary models.
#' @param pi_1 Numeric vector. Propensity scores for the treatment group.
#' @param pi_0 Numeric vector. Propensity scores for the control group.
#' @param Pi_a Numeric vector. Propensity scores from auxiliary models.
#'
#' @return A numeric value representing the randomized ATE (SSL-based estimate).
#' @export
#'
random_ate <- function(Y, A, mu_a, m_a, pi_1, pi_0, Pi_a){

  ate_result_SSL <- Y+ A+ mu_a+ m_a+ pi_1+ pi_0+ Pi_a

  return(ate_result_SSL)
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



#' @export
semi.eff.ate = function(R,Y,trt, or1, or0,
                        psA1,psA0, psR,
                        impA1, impA0, impY1,impY0)
{
  infl = or1 + impA1/psA1*(impY1-or1) - or0 - impA0/psA0*(impY0-or0)
  pos.label = which(R==1)
  infl[pos.label] = (infl[pos.label]+
                       (trt[pos.label]*Y[pos.label]-impA1[pos.label]*impY1[pos.label]
                        -or1[pos.label]*(trt[pos.label]-impA1[pos.label]))/
                       (psA1[pos.label]*psR[pos.label])
                     -((1-trt[pos.label])*Y[pos.label]-impA0[pos.label]*impY0[pos.label]
                       -or0[pos.label]*((1-trt[pos.label])-impA0[pos.label]))/
                       (psA0[pos.label]*psR[pos.label]))

  return(list(ate = mean(infl),
              se = sqrt(var(infl)/length(R))))
}


#' @export
super.ate = function(R,Y,trt, or1, or0,
                     psA1,psA0,psR)
{
  infl = or1 - or0
  pos.label = which(R==1)
  infl[pos.label] = (infl[pos.label] + trt[pos.label]/psA1[pos.label]/psR[pos.label]*(Y[pos.label]-or1[pos.label])
                     - (1-trt[pos.label])/psA0[pos.label]/psR[pos.label]*(Y[pos.label]-or0[pos.label]))

  return(list(ate = mean(infl),
              se = sqrt(var(infl)/length(R))))
}
