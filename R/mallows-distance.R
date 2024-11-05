#' Mallows Distance Between Two Vectors
#'
#' Calculate the \eqn{p^{\text{th}}} Mallows distance of two vectors, also known as the Wasserstein-\eqn{p} distance.
#'
#'
#' @param vec1 A numeric vector.
#' @param vec2 A numeric vector of the same length as \code{vec1}.
#' @param p A number \eqn{\geq 1} for the \eqn{p^{\text{th}}} Mallows distance. Default is 2.
#' @return A numeric value representing the \eqn{p^{\text{th}}} Mallows distance between \code{vec1} and \code{vec2}.
#' @details
#'
#'
#' Calculate the \eqn{p^{\text{th}}} Mallows distance of two vectors, also known as the Wasserstein-\eqn{p} distance between two samples of the same size.
#'
#' Say \eqn{X_i \overset{\text{iid}}{\sim} F} and \eqn{Y_j \overset{\text{iid}}{\sim} G}.
#' Let \eqn{\hat{F}(x) = \sum_i\boldsymbol{1}(X_i \leq x)} and
#' \eqn{\hat{F}^{-1}(t) = \text{inf}\{x : \hat{F}(x) \leq t\}}.
#' Define \eqn{\hat{G}(y)} and \eqn{\hat{G}^{-1}(t)} similarly.
#'
#' Then the \eqn{p^{\text{th}}} Mallows distance between \eqn{\hat{F}(x)} and \eqn{\hat{G}(t)}  is
#'
#' \eqn{\Psi_{p} (\hat{F}, \hat{G}) = \left(\int_{0}^{1} |\hat{F}^{-1}(t) - \hat{G}^{-1}(t) |^p dt \right)^{1/p}}
#'
#' @examples
#' data("karate")
#  # Latent Space Bootstrap
#' set.seed(1)
#' boot.sample <- bootstrap_latent_space(karate, d = 2, B = 1)
#'
#' orig.degree <- get_centrality(karate, "degree")
#' boot.degree <- get_centrality(boot.sample[[1]], "degree")
#'
#' mallows_distance(orig.degree, boot.degree)
#'
#' @seealso [mallows_distance_trimmed()]
#'
#' @references
#'
#' \insertRef{mallows-1972}{JaB}
#'
#' \insertRef{munk-czado-1998}{JaB}
#' @export
mallows_distance <- function(vec1, vec2,p=2 ){

  n <-length(vec1)

  if(n != length(vec2)){stop("vec1 and vec2 must be the same length")}

  Xsort <- sort(vec1)
  Ysort <- sort(vec2)

  ret <- (sum(abs(Xsort-Ysort)^p)/(n))^(1/p)
  return(ret)

}


#' Trimmed Mallows Distance Between Two Vectors
#'
#' Calculate the trimmed \eqn{p^{\text{th}}} Mallows distance of two vectors, also known as the trimmed Wasserstein-\eqn{p} distance.
#'
#'
#' @param vec1 A numeric vector.
#' @param vec2 A numeric vector of the same length as \code{vec1}.
#' @param p A number \eqn{\geq 1} for the \eqn{p^{\text{th}}} Mallows distance. Default is 2.
#' @param alpha a trimming parameter in \eqn{[0, 0.5)}. If `alpha`=0 (default), there is no trimming and result is just the Mallows distance.
#' @return A numeric value representing the \eqn{p^{\text{th}}} Mallows distance between \code{vec1} and \code{vec2}.
#' @details
#'
#'
#' Calculate the trimmed \eqn{p^{\text{th}}} Mallows distance of two vectors, also known as the trimmed Wasserstein-\eqn{p} distance between two samples of the same size.
#'
#' Say \eqn{X_i \overset{\text{iid}}{\sim} F} and \eqn{Y_j \overset{\text{iid}}{\sim} G}.
#' Let \eqn{\hat{F}(x) = \sum_i\boldsymbol{1}(X_i \leq x)} and
#' \eqn{\hat{F}^{-1}(t) = \text{inf}\{x : \hat{F}(x) \leq t\}}.
#' Define \eqn{\hat{G}(y)} and \eqn{\hat{G}^{-1}(t)} similarly.
#'
#' Let \eqn{\alpha\in[0,0/5)} be a trimming parameter.
#'
#' Then the trimmed \eqn{p^{\text{th}}} Mallows distance between \eqn{\hat{F}(x)} and \eqn{\hat{G}(t)}  is
#'
#' \eqn{\Psi_{\alpha, p}(\hat{F}, \hat{G}) = \left( \frac{1}{1-2\alpha}\int_{\alpha}^{1-\alpha} |\hat{F}^{-1}(t) - \hat{G}^{-1}(t) |^p dt \right)^{1/p}}
#'
#' @examples
#' data("karate")
#  # Latent Space Bootstrap
#' set.seed(1)
#' boot.sample <- bootstrap_latent_space(karate, d = 2, B = 1)
#'
#' orig.degree <- get_centrality(karate, "degree")
#' boot.degree <- get_centrality(boot.sample[[1]], "degree")
#'
#' mallows_distance(orig.degree, boot.degree)
#' mallows_distance_trimmed(orig.degree, boot.degree, alpha = 1/34)
#' mallows_distance_trimmed(orig.degree, boot.degree, alpha = 2/34)
#'
#' @seealso [mallows_distance()]
#'
#' @references
#'
#' \insertRef{mallows-1972}{JaB}
#'
#' \insertRef{munk-czado-1998}{JaB}
#' @export
mallows_distance_trimmed <- function(vec1, vec2,p=2, alpha=0 ){

  Xsort <- sort(vec1)
  Ysorted <- sort(vec2)

  # Calculate the number of elements to trim
  n <- length(Xsort)
  trim_count <- max(floor(alpha * n), 1)
  alpha.corrected <- trim_count / n

  # Trim both vectors by keeping the middle (1 - 2a) proportion
  i <- (trim_count + 1):(n - trim_count)
  Xtrim <- Xsort[i]
  Ytrim <- Ysorted[i]

  ret <- (sum(abs(Xtrim-Ytrim)^p)/(n) / (1-2*alpha.corrected))^(1/p)
  return(ret)

}
