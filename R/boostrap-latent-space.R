#' Calculate the Adjacency Spectral Embedding of a Matrix
#'
#' CITE Sussman et al. (2012) paper
#'
#' @param network An igraph object with \eqn{n} nodes.
#' @param d number of latent dimensions to embed the node positions in.
#' @returns An \eqn{n}-by-\eqn{d} matrix where the \eqn{i^{th}} row of the matrix corresponds to node \eqn{i}'s position in the \eqn{d}-dimensional latent space.
#' Bootstrap samples will retain vertex names of the input `network` but will loose all other vertex and edge attributes.
#' @examples
#' #ADD EXAMPLE ONCE DATA IS ADDED
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
#' Add explanation of how latent space bootstrap works
#'
#' Cite Sussman et al. (2012) paper for ASE
#' CITE Levin and Levina (2022) paper for bootstrap procedure.
#' Link to Matrix package
#'
#' @param network An igraph object with \eqn{n} nodes.
#' @param d Dimension of latent space. Recommended only 2 or 3 to maintain interpretability.
#' @param B number of bootstrap samples to calculate
#' @param output.type The class of object the resulting bootstrap networks should be.
#' The default is `igraph` which will make bootstrap samples of class "igraph". Note that for large \eqn{B}, this may not be an efficient use of storage space.
#' Other options include `matrix` which will return bootstrap samples as an \eqn{n}-by-\eqn{n} adjacency matrix,
#' `dgCMatrix` which will return scarce matrices (package `Matrix` must be loaded).
#' @returns A list of length \eqn{B} where each element is an bootstrap sample.
#' Each element is of class `output.type`.
#' @examples
#' # bootstrap_latent_space(paul.igraph, d=2, B=10, output.type = "matrix")
#'
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

