#' Adjacency Spectral Embedding
#'
#' Calculate the adjacency spectral embedding of a network.
#'
#' @param network An igraph object with \eqn{n} nodes.
#' @param d number of latent dimensions to embed the node positions in.
#' @returns An \eqn{n}-by-\eqn{d} matrix where the \eqn{i^{th}} row of the matrix corresponds to node \eqn{i}'s position in the `d`-dimensional latent space.
#' Bootstrap samples will retain vertex names of the input `network` but will loose all other vertex and edge attributes.
#' @references \insertRef{sussman-et-al-2012}{JaB}
#'
#' @details
#'
#' This calculates the latent positions of the nodes of a network in \eqn{\mathbb{R}^d}
#' using adjacency spectral embedding \insertCite{sussman-et-al-2012}{JaB}.
#'
#' Let \eqn{\boldsymbol{A}} be the adjacency matrix of `network` with \eqn{n} nodes.
#' Additionally, let \eqn{\hat{\boldsymbol{\Lambda}} \in \mathbb{R}^{d \times d}}
#' be the diagonal matrix formed by the top `d` largest-magnitude eigenvalues
#' of the adjacency matrix and \eqn{\hat{\boldsymbol{U}} \in \mathbb{R}^{n \times d}}
#' be the matrix with the corresponding eigenvectors as its columns.
#' The adjacency spectral embedding of \eqn{\boldsymbol{A}} is
#' \eqn{\hat{\boldsymbol{X}} = \hat{\boldsymbol{U}}\hat{\boldsymbol{\Lambda}}^{1/2} \in \mathbb{R}^{n \times d}}.
#'
#' The \eqn{\text{i}^{\text{th}}} row of \eqn{\hat{\boldsymbol{X}}} corresponds to the
#' location of the \eqn{\text{i}^{\text{th}}} node in the `d` dimensional latent space.
#'
#'
#'
#' @inherit bootstrap_latent_space examples
#'
#' @export
ASE <- function(network, d){

  A <- igraph::as_adjacency_matrix(network, type = "both", sparse = F)
  e <- base::eigen(A)
  vecs <- e$vectors[, 1:d]
  vals <- e$values[1:d]

  S.half <- sqrt(diag(vals))

  ase.mat <- vecs %*% S.half
  return(ase.mat)

}

#' Latent Space Bootstrap
#'
#' Generate bootstrap samples of a network with the latent space bootstrap method described in \insertCite{levin-levina-2019;textual}{JaB}.
#'
#' @details
#'
#' Generate bootstrap samples of a network with the latent space bootstrap method described in \insertCite{levin-levina-2019;textual}{JaB}.
#'
#' Latent positions of the nodes in the network are first calculated in \eqn{\mathbb{R}^d} using adjacency spectral with \link[JaB:ASE]{ASE} \insertCite{sussman-et-al-2012}{JaB}.
#' Then for each pair of nodes, an edge is present with probability equal to the dot product of their positions in the latent space.
#'
#' Let \eqn{\boldsymbol{A}} be the adjacency matrix of `network` with \eqn{n} nodes.
#' Additionally, let \eqn{\hat{\boldsymbol{\Lambda}} \in \mathbb{R}^{d \times d}}
#' be the diagonal matrix formed by the top `d` largest-magnitude eigenvalues
#' of the adjacency matrix and \eqn{\hat{\boldsymbol{U}} \in \mathbb{R}^{n \times d}}
#' be the matrix with the corresponding eigenvectors as its columns.
#' The adjacency spectral embedding of \eqn{\boldsymbol{A}} is
#' \eqn{\hat{\boldsymbol{X}} = \hat{\boldsymbol{U}}\hat{\boldsymbol{\Lambda}}^{1/2} \in \mathbb{R}^{n \times d}}.
#'
#' Let \eqn{X_i} be the \eqn{\text{i}^{\text{th}}} row of \eqn{\hat{\boldsymbol{X}}},
#' which corresponds to the
#' location of the \eqn{\text{i}^{\text{th}}} node in the `d` dimensional latent space.
#'
#' To generate a bootstrap adjacency matrix \eqn{\boldsymbol{A}^{(b)}}, for
#' \eqn{1 \leq i < j \leq n} independently draw \eqn{A_{ij}^{(b)} \sim \text{Bernoulli}(X_i^T X_j)}.
#'
#' There may be cases in which \eqn{X_i^T X_j \not\in [0,1]}. In these cases, replace
#' \eqn{X_i^T X_j} with \eqn{\max(\min(X_i^T X_j, 1), 0)}.
#'
#' @param network An igraph object with \eqn{n} nodes.
#' @param d Dimension of latent space. Recommended only 2 or 3 to maintain interpretability.
#' @param B number of bootstrap samples to calculate
#' @param output.type The class of object the resulting bootstrap networks should be.
#' The default is `igraph` which will make bootstrap samples of class "igraph". Note that for large \eqn{B}, this may not be an efficient use of storage space.
#' Other options include `matrix` which will return bootstrap samples as an \eqn{n}-by-\eqn{n} adjacency matrix,
#' `dgCMatrix` which will return scarce matrices (package `Matrix` must be loaded), or
#' `edgelist` which will return a \eqn{n}-by-\eqn{2} matrix of the list of edges in the network.
#' See \link[JaB:make_network_type]{make_network_type} for details.
#' @returns A list of length \eqn{B} where each element is an bootstrap sample.
#' Each element is of type `output.type`.
#'
#' @references \insertAllCited{}
#'
#'
#' @examples
#' library(igraph)
#' data("karate")
#' #Find the latent positions in 2D
#' X <- ASE(karate, 2)
#' plot(X)
#'
#' # Color According to faction
#' plot(X, col = ifelse(V(karate)$Faction == 1, "red", "blue"))
#'
#' # Latent Space Bootstrap
#' set.seed(1)
#' boot.sample <- bootstrap_latent_space(karate, d = 2, B = 1)
#'
#' #plot comparison of original data and bootstrap sample
#' par(mfrow = c(1, 2))
#' l <- igraph::layout_nicely(karate)
#' plot(karate,
#'      layout = l,
#'      main = "Karate Data")
#' plot(boot.sample[[1]],
#'      layout = l,
#'      main = "Karate Bootstrap Sample",
#'      vertex.label = 1:gorder(karate),
#'      vertex.color = V(karate)$color)
#' @export
bootstrap_latent_space <- function(network, d=2, B=1000,
                                   output.type = "igraph"){

  #if using output.type == "dgCMatrix", package Matrix must be installed
  if(output.type == "dgCMatrix"){
    if(is.element("Matrix", utils::installed.packages()[, "Package"])){
      stop("To use output.type = 'dgCMatrix', the package 'Matrix' must be installed.")
    }
  }

  #get node names
  names <- igraph::V(network)$name #is null if no names

  # Get latent position
  Z <- ASE(network, d)

  #get bootstrap sample
  n <- base::nrow(Z)
  P <- base::matrix(NA, nrow = n, ncol = n)

  #Calculate Probabilities
  P <-  Z %*% base::t(Z)
  P[base::lower.tri(P, diag = TRUE)] <- NA
  P <-ifelse(P>1, 1, P)
  P <-ifelse(P<0, 0, P)

  # Get bootstrap samples
  ret <- base::vector(mode = "list", length = B)

  for(b in 1:B){
    #get adjacency matrix
    A <- base::matrix(0, n, n)
    base::colnames(A) <- base::rownames(A) <- names

    # Get the upper triangle indices and sample from binomial with respective probabilites
    upper_indices <- base::which(upper.tri(P))
    A[upper_indices] <- stats::rbinom(length(upper_indices), 1, P[upper_indices])
    ret[[b]] <- A + base::t(A) #makes a symmetric adjacency matrix

  }

  ret <- make_network_type(ret, "matrix", output.type)
  return(ret)

}

