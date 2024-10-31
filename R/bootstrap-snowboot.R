#' Sample about one seed from `snowboot` package
#'
#' CITE snowboot package
#'
#' This is a slightly modified version of the sample_about_one_seed function from the snowboot package.
#' This function uses the node names, not node IDs,
#' and it returns the edge list of the bootstrap sample, not the nodes included
#'
#' @param net A network object as described in the snowboot package
#' @param seed Selected seed to start the LSMI (name of the node)
#' @param n.wave Number of waves to be included each the snowball sample
#' @returns A list of length `n.wave` where each element is the edge list of edges included on the corresponding wave of sampling.
#' @examples
#' #ADD EXAMPLE ONCE DATA IS ADDED

sample_about_one_seed_modified <- function (net, seed, n.wave = 1) {
  if (n.wave < 1) {
    stop("Number of waves, n.wave, should be >= 1.")
  }
  effEdges <- net$edges
  keepEdges <- vector(mode = "list", length = n.wave) #this will be used to create the network
  nodes.waves <- as.list(c(seed, rep(NA, n.wave)))
  wave <- 1
  while (wave <= n.wave & nrow(effEdges) >= 0) {
    tmp <- is.element(effEdges, nodes.waves[[wave]])
    if (any(tmp)) {
      tmp <- which(matrix(tmp, dim(effEdges)[1], 2), arr.ind = TRUE)
      nodes.waves[[wave + 1]] <- sort(effEdges[cbind(tmp[,1], sapply(tmp[, 2], FUN = switch, 2, 1))])
      keepEdges[[wave]] <- matrix(effEdges[tmp[,1],], ncol = 2)
      effEdges <- effEdges[-tmp[, 1], ]
      if (is.vector(effEdges)) {
        effEdges <- t(effEdges)
      }
    }
    wave <- wave + 1
  }

  return( lapply(keepEdges, as.data.frame))
}


#' Snowball Sampling (Labeled Snowball with multiple inclusions)
#'
#' CITE snowboot package
#' CITE Gel et al paper + Thompson et al paper
#'
#' This is a modified version of the lsmi function in the snowboot package.
#' This function uses the node names, not node IDs,
#' and it returns the edge list of the bootstrap sample, not the nodes included
#'
#' @param network An igraph object with \eqn{n} nodes.
#' @param num.seed Number of seeds to be included each the snowball sample
#' @param num.wave Number of waves to be included each the snowball sample
#' @param B Number of bootstrap samples
#' @param output.type The class of object the resulting bootstrap networks should be.
#' The default is `igraph` which will make bootstrap samples of class "igraph". Note that for large \eqn{B}, this may not be an efficient use of storage space.
#' Other options include `edgelist` which returns an edge list for each bootstrap sample,
#'  `matrix` which returns bootstrap samples as an \eqn{n}-by-\eqn{n} adjacency matrix,
#' `dgCMatrix` which returns sparse matrices (package `Matrix` must be loaded).
#' @returns A list of length \eqn{B} where each element is an bootstrap sample.
#' Each element is of class `output.type`.
#' @examples
#' #ADD EXAMPLE ONCE DATA IS ADDED
#' @export

bootstrap_snowboot <- function(network, B,
                               num.seed = NA, num.wave = NA,
                               output.type = "igraph"){

  net <- snowboot::igraph_to_network(network)

  net[["names"]] <- get_nodes(network)
  ### modified code from snoboot::lsmi and snowboot::sample_about_one_seed
  ### Added use of names in network object and new output types

  boot_edge_lists <- vector(mode = "list", length = B)
  for(b in 1:B){
    #get sample (using node names)
    sample_seeds <- net$name[sort(sample(1:net$n, num.seed, replace = FALSE))]
    #get resulting lsmi sample as an edge list
    edge_list <- lapply(sample_seeds, function(x) JaB:::sample_about_one_seed_modified(net, x, num.wave))
    #combine into one edge list
    boot_edge_lists[[b]] <- as.matrix(dplyr::distinct(dplyr::bind_rows(lapply(edge_list, dplyr::bind_rows))))
  }

  ret <- try(make_network_type(boot_edge_lists, "edgelist", output.type))
  return(ret)
}
