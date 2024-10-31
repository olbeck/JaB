#' Vertex Bootstrap Sampling for networks
#'
#' Generates bootstrap samples of a network by resampling vertices. This function uses a modified version of the `vertboot` function from the `snowboot` package to allow flexible input and output network types.
#'
#' @param network An igraph object representing the original network.
#' @param B An integer specifying the number of bootstrap samples to generate. Defaults to 1000.
#' @param output.type A character string specifying the format of the output networks. Options include `"igraph"`, `"matrix"`, `"dgCMatrix"`, or `"edgelist"`. Defaults to `"igraph"`.
#'
#' @return A list of bootstrap sample networks, each in the specified `output.type` format.
#'
#' @details
#' The function performs vertex bootstrapping on the adjacency matrix of the input `network`, generating `B` bootstrap samples. Each sampled adjacency matrix is assigned row and column names based on the original vertex names. The function then converts the sampled matrices to the specified `output.type`.
#' CITE VERTEXT BOOTSTRAP PAPER, CITE THOMPSON ET AL, CITE SNOWBOOT
#'
#' @examples
#' # Create a sample igraph network
#' network <- igraph::make_ring(10)
#'
#' # Generate bootstrap samples as igraph objects
#' boot_samples <- bootstrap_vertex(network, B = 100, output.type = "igraph")
#'
#' # Generate bootstrap samples as adjacency matrices
#' boot_samples_matrices <- bootstrap_vertex(network, B = 100, output.type = "matrix")
#'
#' @seealso \code{\link{make_network_type}}, \code{\link[snowboot]{vertboot}}
#' @importFrom igraph as_adjacency_matrix V gorder
#' @export
bootstrap_vertex <- function(network, B=1000, output.type = "igraph"){

  adj <- igraph::as_adjacency_matrix(network, sparse=FALSE)
  names <- igraph::V(network)$name
  if(is.null(names)){names <- 1:igraph::gorder(network)}
  samp_networks <- snowboot::vertboot(adj, B)
  samp_networks <-
    lapply(samp_networks,
      function(mat) {
       rownames(mat) <- names
       colnames(mat) <- names
       mat})

  ret <- make_network_type(samp_networks, "matrix", output.type)
  return(ret)
}

