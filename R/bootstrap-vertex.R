#' Vertex Bootstrap Sampling for networks
#'
#' Generate bootstrap samples of a network by resampling vertices.
#' This function uses a modified version of the \link[snowboot:vertboot]{vertboot} from the \pkg{snowboot} package to allow flexible input and output network types.
#'
#' @param network An igraph object representing the original network.
#' @param B An integer specifying the number of bootstrap samples to generate. Defaults to 1000.
#' @param output.type A character string specifying the format of the output networks. Options include `"igraph"`, `"matrix"`, `"dgCMatrix"`, or `"edgelist"`. Defaults to `"igraph"`. See \link[JaB:make_network_type]{make_network_type} for details.
#'
#' @return A list of bootstrap sample networks, each in the specified `output.type` format.
#'
#' @details
#'
#' Generates bootstrap samples of a network by resampling vertices.
#' This function uses a modified version of the \link[snowboot:vertboot]{vertboot} from the \pkg{snowboot} \insertCite{snowboot}{JaB} package to allow flexible input and output network types.
#'
#' This procedure is first described in \insertCite{snijders-borgatti-1999;textual}{JaB}
#' and is outlined explicitly in \insertCite{chen-et-al-2018;textual}{JaB}.
#'
#' @references \insertAllCited{}
#'
#' @examples
#' library(igraph)
#' data(karate)
#' set.seed(12)
#' boot.samp <- bootstrap_vertex(karate, B=1)
#'
#' #plot comparison of original data and bootstrap sample
#' par(mfrow = c(1, 2))
#' #get the same positions in the original data and bootstrap samples
#' l <- igraph::layout_nicely(karate)
#' which.index <- NA
#' for(i in 1:gorder(boot.samp[[1]])){
#'   which.index[i] <- which(V(boot.samp[[1]])$name[i] == V(karate)$name)
#' }
#'
#' plot(karate,
#'      layout = l,
#'      main = "Karate Data",
#'      vertex.label = NA)
#' plot(boot.samp[[1]],
#'      layout = l[which.index, ],
#'      main = "Karate Bootstrap Sample",
#'      vertex.label = NA,
#'      vertex.color = V(karate)$Faction[which.index ])
#'
#'
#' @seealso \code{\link{make_network_type}}, \code{\link[snowboot]{vertboot}}
#' @importFrom igraph as_adjacency_matrix V gorder
#' @export
bootstrap_vertex <- function(network, B=1000, output.type = "igraph"){

  adj <- igraph::as_adjacency_matrix(network, sparse=FALSE)
  names <- get_nodes(network)
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

