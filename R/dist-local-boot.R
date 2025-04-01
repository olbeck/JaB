# ## Alternate distance functions for the localboot
#
#
#
# ## ICE distance is found here https://github.com/Siva-47/ICE/blob/main/methods/ICE.R
# source("https://raw.githubusercontent.com/Siva-47/ICE/refs/heads/main/methods/ICE.R")
#
# dist_ICE_ij <- function(Phat, i,j){
#   n <- nrow(Phat)
#   inner <- sapply(
#     (1:n)[-c(i,j)],
#     function(k){(Phat[i, k] - Phat[j,k])^2 }
#   )
#   return(sqrt(sum(inner)))
# }
#
# dist_ICE <- function(A, delta_0=0.5){
#
#   n <- nrow(A)
#   P.hat <- ICE(A, C_it = 1, C_est=1, P_hat_0 = NULL, delta_0)
#   P.hat <- P.hat$final_P_hat
#
#   D <- matrix(0, n, n)
#   for(i in 1:(n-1)){
#     for(j in (i+1):n){
#       D[i,j] <- dist_ICE_ij(P.hat, i, j)
#       D[j,i] <- D[i,j]
#     }
#   }
#
#   return(D)
#
# }


# from Estimating network edge probabilities by neighborhood smoothing
# BYYUAN ZHANG, ELIZAVETA LEVINA, AND JI ZHU

#' Upper Bound distance
#'
#' Compute the upper bound distance defined in \insertCite{zhang-levina-zhu-2017;textual}{JaB}.
#'
#'
#' @param A An \eqn{n \times n} adjacency matrix
#'
#' @return An \eqn{n \times n} matrix \eqn{D}, where \eqn{D_{i,j}} represents the computed distance
#'         between nodes \eqn{v_i} and \eqn{v_j}.
#'
#'
#' @details
#'
#' Computes distance matrix \eqn{D} where each
#' \eqn{d_{i,j} = \max_{k\neq i,j}| \langle A_{i, \cdot} - A_{j, \cdot} , A_{k, \cdot} \rangle | /n}
#'
#' \eqn{d_{i,j}} is computed by `dist_max_ij` internal function.
#'
#' @examples
#' library(JaB)
#' library(igraph)
#' data("paul.revere")
#'
#' A <- as_adjacency_matrix(paul.revere, sparse = FALSE)
#' D <- dist_max(A)
#'
#' @references \insertAllCited{}
#'
#' @export
dist_max <- function(A){
  n <- nrow(A)
  D <- matrix(0,n,n)
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      D[i,j] <- dist_max_ij(A, i, j)
      D[j,i] <- D[i,j]
    }
  }
  return(D)
}


#' Internal function for Upper Bound distance d_ij
dist_max_ij <- function(A, i, j) {
  n <- nrow(A)
  diff <- A[i, ] - A[j, ]
  inner <- sapply(
    (1:n)[-c(i,j)],
    function(k){ abs(sum(diff * A[k, ]))/n}
  )
  return(max(inner))
}



## Euclidiean distance of adjacency spectal embeddings


#' Euclidean distance of adjacency spectral embeddings
#'
#' Compute the Euclidean distance of adjacency spectral embeddings
#'
#'
#' @param A An \eqn{n \times n} adjacency matrix
#' @param d dimension of the latent space embed in
#'
#' @return An \eqn{n \times n} matrix \eqn{D}, where \eqn{D_{i,j}} represents the distance
#'         between nodes \eqn{v_i} and \eqn{v_j}.
#'
#' @details
#' For adjacency matrix \eqn{\boldsymbol{A}} with \eqn{n} nodes,
#' let \eqn{\hat{\boldsymbol{\Lambda}} \in \mathbb{R}^{d \times d}}
#' be the diagonal matrix formed by the top `d` largest-magnitude eigenvalues
#' of the adjacency matrix and \eqn{\hat{\boldsymbol{U}} \in \mathbb{R}^{n \times d}}
#' be the matrix with the corresponding eigenvectors as its columns.
#' The adjacency spectral embedding of \eqn{\boldsymbol{A}} is
#' \eqn{\hat{\boldsymbol{X}} = \hat{\boldsymbol{U}}\hat{\boldsymbol{\Lambda}}^{1/2} \in \mathbb{R}^{n \times d}}.
#'
#' Then, \eqn{D_{i,j}} is the Euclidean norm of \eqn{\hat{\boldsymbol{X}}_{i, \cdot}} and \eqn{\hat{\boldsymbol{X}}_{j, \cdot}}.
#'
#' @examples
#' library(JaB)
#' library(igraph)
#' data("paul.revere")
#'
#' A <- as_adjacency_matrix(paul.revere, sparse = FALSE)
#' D <- dist_ASE(A)
#'
#' @export
dist_ASE <- function(A, d=2){

  e <- base::eigen(A)
  vecs <- e$vectors[, 1:d]
  vals <- e$values[1:d]

  S.half <- sqrt(diag(vals))

  ase.mat <- vecs %*% S.half

  D <- as.matrix(proxy::dist(ase.mat, method = "euclidean"))

  return(D)
}


#
# P = generate_graphon(10, 1)
# A = generate_network_P(P, replicate = 1, symmetric.out = TRUE,noloop=TRUE)
# dist_max(A)
# dist_ASE(A)
# dist_ICE(A, delta_0 = 0.1)
#
# localboot(A, B=1, dist_func = dist_max)
# localboot(A, B=1, dist_func = dist_ASE)
# localboot(A, B=1, dist_func = dist_ICE)
#
