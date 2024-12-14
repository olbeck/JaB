#' Equivalence Test of Two Vectors Using Mallows Distance
#'
#' Conduct the equivalence test using the \eqn{p^{\text{th}}} Mallows distance found in \insertCite{munk-czado-1998;textual}{JaB}
#'
#' @param vec1 A numeric vector.
#' @param vec2 A numeric vector of the same length as \code{vec1}.
#' @param alpha Trimming parameter \eqn{\alpha \in(0, 0.5)} for trimmed Mallows' distance.
#' @param delta0 Tolerance value for hypothesis test.
#' @details
#'
#' Add details here
#'
#'
#'
mallow_equiv_test <- function(vec1, vec2, alpha, delta0){

  m <- length(Xdist)
  n <- length(Ydist)

  N <- max(n, m)

  a <- max(floor(alpha * N), 1)
  alpha <- a/ N

  coeff <- sqrt( (n*m) / (n+m) )

  num <- (Gamma_alpha(Xdist, Ydist, alpha))^2 - delta0^2

  test.stat <- coeff * num

  p.val <- pnorm(TS)
}
