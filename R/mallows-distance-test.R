#' Variance Estimate for Mallows Distance Equivalence Test
#'
#' Calculated estimated variance for Mallows distance equivalence test distance found in \insertCite{munk-czado-1998;textual}{JaB}
#'
#' @param vec1 A numeric vector.
#' @param vec2 A numeric vector of the same length as \code{vec1}.
#' @param alpha Trimming parameter \eqn{\alpha \in(0, 0.5)} for trimmed Mallows' distance.
#' @return The estimated variance used in the Mallow equivalence test.
#'
#' @details
#' Using the same set up as described in [`mallows_equiv_test`], `sigma2_hat_alpha`
#' calculates a consistent estimator of
#' \eqn{\sigma^2_{\alpha}(F, G) }. See `vignette("mallows-equiv-test")` or Appendix A of
#' \insertCite{munk-czado-1998;textual}{JaB} for its explicit expression.
#'
#' @seealso \link{[mallows_equiv_test()]}
#'
#' @noRd
sigma2_hat_alpha <- function(vec1, vec2, alpha){

  Xdist <- sort(vec1)
  Ydist <- sort(vec2)

  n <- m <- length(Xdist)

  a <- max(floor(alpha * m), 1)
  alpha <- a/ m

  lmbd <- n / (n+m)

  p1 <- lmbd * ( T2_alpha(Xdist, Ydist, alpha) - T1_alpha(Xdist, Ydist, alpha)^2 )

  p2 <- (1 - lmbd) * ( T2_alpha(Ydist, Xdist, alpha) - T1_alpha(Ydist, Xdist, alpha)^2 )

  return(4 * ( p1 + p2 ) / ((1-2*alpha)^4) )

}

#' T1 internal function for \link{[sigma2_hat_alpha()]}
#'
#' @param vec1 A numeric vector.
#' @param vec2 A numeric vector of the same length as \code{vec1}.
#' @param alpha Trimming parameter \eqn{\alpha \in(0, 0.5)} for trimmed Mallows' distance.
#'
#' @details
#' Internal function for [sigma2_hat_alpha()]
#'
#' @noRd
T1_alpha <- function(Xdist, Ydist, alpha){

  Xsort <- sort(Xdist)
  Ysort <- sort(Ydist)

  m <- n <- length(Ysort)

  a <- max(floor(alpha * m), 1)
  alpha <- a/ m

  p1 <- alpha * 1/2 * (Xsort[n-a]^2- Xsort[a]^2)

  index2 <- a:(n-a-1)
  p2 <- alpha * sum(Ysort[index2]*(Xsort[index2+1] - Xsort[index2]))

  p3 <- Xsort[n-a]^2 * (1-2*alpha)/2

  index4 <- (a+1):(n-a-1)
  p4 <- 1/(2*n) * sum(Xsort[index4])

  p5 <- 0
  for(k in (a+1):(n-a-1)){
    for(l in k:(n-a-1)){
      p5 <- p5 + ( Ysort[l] * (Xsort[l+1] - Xsort[l]) )
    }
  }
  p5 <- p5/n

  return( p1 - p2 + p3 - p4 - p5)
}

#' T3 internal function for \link{[sigma2_hat_alpha()]}
#'
#' @param vec1 A numeric vector.
#' @param vec2 A numeric vector of the same length as \code{vec1}.
#' @param alpha Trimming parameter \eqn{\alpha \in(0, 0.5)} for trimmed Mallows' distance.
#'
#' @details
#' Internal function for [sigma2_hat_alpha()]
#'
#' @noRd
T3_alpha <- function(Xdist, Ydist, alpha){
  T1_alpha(Ydist, Xdist, alpha)
}

#' T2 internal function for \link{[sigma2_hat_alpha()]}
#'
#' @param vec1 A numeric vector.
#' @param vec2 A numeric vector of the same length as \code{vec1}.
#' @param alpha Trimming parameter \eqn{\alpha \in(0, 0.5)} for trimmed Mallows' distance.
#'
#' @details
#' Internal function for [sigma2_hat_alpha()]
#'
#' @noRd
T2_alpha <- function(Xdist, Ydist, alpha) {
  Xsort <- sort(Xdist)
  Ysort <- sort(Ydist)

  m <- length(Xsort)
  n <- length(Ysort)

  a <- max(floor(alpha * m), 1)
  alpha <- a/ m


  index1 <- a:(n-a-1)
  p1 <- alpha * (
    (1/2) * (Xsort[n-a]^2 - Xsort[a]^2) -
      sum(Ysort[index1] * (Xsort[index1+1] - Xsort[index1]) )
  )^2

  index2 <- (a+1):(n-a-1)
  p2 <- sum( (Xsort[n-a]^2 - Xsort[index2]^2) ^2 ) / (4*n)

  p3 <- 0
  for(j in (a+1):(n-a-1)){
    p3.1 <- Xsort[n-a]^2 - Xsort[j]^2
    for(i in j:(n-1-a)){
      p3 <- p3 + p3.1 * Ysort[i] * (Xsort[i+1] - Xsort[i])
    }
  }
  p3 <- p3 /n

  p4 <- 0
  for(j in (a+1):(n-a-1)){
    for(i in j:(n-a-1)){
      p4 <- p4 + Ysort[i] * (Xsort[i+1] - Xsort[i])
    }
  }
  p4 <- p4/n

  return(p1 + p2 - p3 + p4)
}


#' T4 internal function for \link{[sigma2_hat_alpha()]}
#'
#' @param vec1 A numeric vector.
#' @param vec2 A numeric vector of the same length as \code{vec1}.
#' @param alpha Trimming parameter \eqn{\alpha \in(0, 0.5)} for trimmed Mallows' distance.
#'
#' @details
#' Internal function for [sigma2_hat_alpha()]
#'
#' @noRd
T4_alpha <- function(Xdist, Ydist, alpha){
  T2_alpha(Ydist, Xdist, alpha)
}


#' Equivalence Test of Two CDFs Using Mallows Distance
#'
#' Conduct the equivalence test of two CDFs using the \eqn{p^{\text{th}}} Mallows distance proposed by \insertCite{munk-czado-1998;textual}{JaB}
#'
#' @param vec1 A numeric vector.
#' @param vec2 A numeric vector of the same length as \code{vec1}.
#' @param alpha Trimming parameter \eqn{\alpha \in(0, 0.5)} for trimmed Mallows' distance.
#' @param delta0 Tolerance value for hypothesis test.
#' @param sig.level Significance level of the test. Default is 0.05.
#' @returns A list with the results of the test:
#' * `dist.hat` is the estimated trimmed Mallows distance between the two distributions, \eqn{\Psi_{\alpha, 2}(\hat{F}, \hat{G})}.
#' * `sd.hat` is the estimate of the standard deviation between the two distributions, \eqn{\hat{\sigma}_{\alpha}}.
#' * `test.stat` is the test statistic.
#' * `pval` is the p-value of the test.
#' * `test.result` is  "REJECT NULL" if `pval`\eqn{\leq}`sig.level` and "FAIL TO REJECT NULL" otherwise.
#' * `ci` is the upper (1-`sig.level`) confidence interval for the square of `dist.hat`
#' * `alpha` is the trimming parameter used in the test. Sometimes this is slightly different than the input `alpha`. This test requires an integer \eqn{a} such that \eqn{\alpha = a/n}. This `alpha` is the smallest \eqn{\alpha} that makes this equation true. See \insertCite{munk-czado-1998;textual}{JaB} for details.
#' * `delta0` is the tolerance parameter used for the test. This is the same as the input `delta0`.
#'
#' @details
#'
#' This is the equivalence test of two CDFs using the trimmed Mallows distance proposed by \insertCite{munk-czado-1998;textual}{JaB}.
#'
#' Say \eqn{X_i \overset{iid}{\sim} F} and \eqn{Y_j \overset{iid}{\sim} G} for \eqn{i,j = 1, ..., n}
#' where \eqn{F} and \eqn{G} are continuous distribution functions.
#' Let \eqn{\hat{F}_n(x) = \frac{1}{n}\sum_{i=1}^n \boldsymbol{1}\{X_i \leq x\}}
#' denote the empirical cumulative distribution function (ECDF) of \eqn{X}, and
#' \eqn{\hat{F}^{-1}_n(t) = \inf\{x : F_n(x) \geq t\}} denote the quantile function.
#' Define \eqn{\hat{G}_m(y)} and \eqn{\hat{G}^{-1}_m(t)} similarly for \eqn{Y}.
#'
#' The Trimmed \eqn{p^{th}} Mallows distance with trimming parameter \eqn{\alpha \in [0, 1/2)} is :
#' \eqn{\begin{equation}
#' \Psi_{\alpha, p}(F, G) =  \frac{1}{1-2\alpha} \left[ \int_{\alpha}^{1-\alpha} | F^{-1}(u) - G^{-1}(u) | ^p du \right]^{1/p}.
#' \end{equation} }
#'
#' \insertCite{munk-czado-1998;textual}{JaB} conduct the equivalence test for some suitable \eqn{0<\Delta_0\in \mathbb{R}}:
#' \eqn{
#' \begin{equation}
#' H_0: \Psi_{\alpha, 2}(F, G) \geq \Delta_0 \quad \text{versus} \quad H_A:  \Psi_{\alpha, 2}(F, G) < \Delta_0
#' \end{equation} }
#'
#' Then, a consistent level \eqn{\alpha^*} test for this hypothesis rejects \eqn{H_0} if,
#' \eqn{
#' \begin{equation}\label{eqn:MallowTest}
#' \left(\frac{nm}{n+m}\right)^{1/2} \frac{\Psi^2_{\alpha_{n \wedge m}, 2}(\hat{F}_n, \hat{G}_n) - \Delta_0^2}{\hat{\sigma}_{\alpha}(F, G)} \leq q_{\alpha^*}
#' \end{equation} }
#'
#' where \eqn{q_{\alpha^*}} is the \eqn{\alpha^*} quantile of the standard normal distribution
#' and \eqn{\hat{\sigma}_{\alpha}} is a consistent estimator of the expected variance between \eqn{F} and \eqn{G}.
#' See `vignette("mallows-equiv-test")` or Appendix A of
#' \insertCite{munk-czado-1998;textual}{JaB} for the explicit expression of \eqn{\hat{\sigma}_{\alpha}}.
#'
#' Currently, this function only runs when \eqn{n=m}. The theory still holds when \eqn{n\neq m}.
#'
#' See `vignette("mallows-equiv-test")` for more details on the construction of this test.
#'
#' @examples
#' set.seed(2935)
#' X <- runif(500)
#' Y <- truncnorm::rtruncnorm(500, a = 0, b = 1, mean = 1/2, sd = 5)
#'
#' # Plot the ECDFs
#' plot(ecdf(X), col = "red")
#' lines(ecdf(Y), col = "blue")
#'
#' # Run the test for trimming parameter 0.05, tolerance value of 0.8 and
#' # significance level of 0.05.
#' test.result <- mallows_equiv_test(X, Y, alpha = 0.05, delta0 = 0.8)
#' test.result
#'
#'
#' @export
#'
mallows_equiv_test <- function(vec1, vec2, alpha, delta0, sig.level = 0.05){

  if(delta0 <= 0){stop("delta0 must be greater than 0.")}
  if(alpha <0 | alpha > 1){stop("alpha must be between 0 and 1.")}

  Xdist <- sort(vec1)
  Ydist <- sort(vec2)

  m <- length(Xdist)
  n <- length(Ydist)
  if(m!=n){stop("vec1 and vec2 must be the same length.")}

  N <- max(n, m)

  a <- max(floor(alpha * N), 1)
  alpha <- a/ N

  coeff <- sqrt( (n*m) / (n+m) )

  dist.hat <- mallows_distance_trimmed(Xdist, Ydist, p=2, alpha=alpha)
  num <- (dist.hat)^2 - delta0^2
  denom <- sqrt(sigma2_hat_alpha(Xdist, Ydist, alpha))

  test.stat <- coeff * num / denom


  pval <- pnorm(test.stat)
  test.result <- ifelse(pval <= sig.level, "REJECT NULL", "FAIL TO REJECT NULL")

  ci <- c(0, (dist.hat)^2 + qnorm(1-sig.level) *denom *sqrt(1/n + 1/m) )


  ret <- list(
    dist.hat = dist.hat,
    sd.hat = denom,
    test.stat = test.stat,
    pval = pval,
    test.result=test.result,
    ci = ci,
    alpha = alpha,
    delta0 = delta0
  )
  return(ret)
}
