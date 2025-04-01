#' Local Bootstrap
#'
#' Generate bootstrap samples of a network with the local bootstrap method described in \insertCite{zu-quin-2025;textual}{JaB}
#' from the \pkg{localboot} package.
#'
#' @details
#'
#' This is a wrapper function to generate bootstrap samples of a network with
#' the local bootstrap method described in \insertCite{zu-quin-2025;textual}{JaB}.
#' This function uses the \link[localboot:localboot]{localboot} function to
#' generate bootstrap samples, then formats them to fit in the JaB package syntax.
#'
#'
#'
#' Let \eqn{\boldsymbol{A}} be the adjacency matrix of `network` \eqn{G } with \eqn{n} nodes
#' in node set \eqn{V}.
#'
#' Let \eqn{\boldsymbol{D}} be the distance matrix for all pairs of nodes where \eqn{d_{i,j}}
#' is the distance between node \eqn{i} and node \eqn{i} according to the distance function `dist.func`.
#'
#' Construct a neighborhood for each node as \eqn{\mathcal{N}(v_i) = \{v_j \in V\ : d_{i,j} < d_i^{(s)} \}}
#' where \eqn{d_i^{(s)} } is the \eqn{s^{th}} smallest distance of \eqn{d_{i,1}, ..., d_{i, n}}.
#' This construction ensures each neighborhood always has \eqn{s = |\mathcal(N)(v_i)|} neighbors.
#'
#'
#' To generate a bootstrap adjacency matrix \eqn{\boldsymbol{A}^{(b)}}, for
#' \eqn{1 \leq i < j \leq n} independently draw
#' \deqn{
#' \begin{align}
#' A_{ij}^{(b)} &\sim \text{Uniform} \Big(\{A_{k\ell} : k \in \mathcal{N}(v_{i}^{(b)}), \ell \in \mathcal{N}(v_{j}^{(b)})\} \Big) \\
#' v_i^{(b)} &\sim \text{Uniform}(V)
#' \end{align}
#' }
#'
#' There may be cases in which \eqn{\mathcal{N}(v_{i}^{(b)})} and \eqn{\mathcal{N}(v_{j}^{(b)})}
#' are overlapping sets. In these cases, set  \eqn{\mathcal{N}(v_{i}^{(b)}) \cap \mathcal{N}(v_{j}^{(b)}) = \emptyset}.
#'
#'
#'
#' @param network An `igraph` object with \eqn{n} nodes.
#' @param B number of bootstrap samples to calculate
#' @param quantile_n Parameter for \link[localboot:localboot]{localboot} function. The quantile used for neighborhood selection in the \link[localboot:localboot]{localboot} function. Defaults to `NULL` which uses \eqn{\sqrt{log(n)/n}}.
#' @param dist.func Parameter for \link[localboot:localboot]{localboot} function. The distance function used to calculate distances between nodes in `network`. Defaults to `get_dist_default_eigen`, an internal function in the \pkg{localboot} package.
#' @param weighted Parameter for \link[localboot:localboot]{localboot} function. A logical indicating if the network is weighted. Defaults to `FALSE`.
#' @param fast Parameter for \link[localboot:localboot]{localboot} function. A logical indicating if a faster, approximate method should be used. Automatically set based on network size if NULL.
#'
#' @returns A list of length `B` where each element is a bootstrapped network as an igraph object.
#' Each bootstrapped network, \eqn{G^{(b)}} has the following vertex attributes:
#' \itemize{
#'   \item `boot.index` is the index of the node in the bootstrap sample as \eqn{vb1, vb2, ..., vbn}.
#'   \item `original.index.cpp` this is the index of the corresponding node in
#'   the original network as \eqn{v0, v1, ..., v(n-1)}.  The \link[localboot:localboot]{localboot}
#'    function uses C++ which is the reason for this index.
#'    For example if \eqn{v_1^{b)}} is the \eqn{4^{th}} node in the original `network`, the `original.index.cpp[1]` is \eqn{v3}.
#'   \item `original.index.r`this is the index of the corresponding node in
#'   the original network as \eqn{v1, v2, ..., vn}.  The \link[localboot:localboot]{localboot}
#'    function uses C++, but we want to use R's indexing from 1 to n.
#'    For example if \eqn{v_1^{b)}} is the \eqn{4^{th}} node in the original `network`, then `original.index.cpp[1]` is \eqn{v4}.
#'   \item `name` is the name of the corresponding node in the original network.
#'   For example if \eqn{v_1^{b)}} is the \eqn{4^{th}} node in the original `network`,
#'   then `name[1]` is `V(network)$name[4]`.
#' }
#'
#' @references \insertAllCited{}
#'
#'
#' @examples
#' library(JaB)
#' library(igraph)
#' library(localboot)
#' data("paul.revere")
#'
#' # generate 10 bootstrap samples
#' bootstrap_local(paul.revere, B = 10)
#'
#' # Use a new distance function
#' @export
bootstrap_local <- function(
    network,
    B = 3000,
    quantile_n = NULL,
    dist.func = dist_ASE,
    weighted = FALSE,
    fast = NULL,
    node.names = NULL){


  #check if network is igraph object
  cl <- class(network)
  if(!("igraph" %in% cl)) {stop("network must be an igraph object")}

  N <- igraph::gorder(network)

  # get node names
  if(is.null(node.names) | length(node.names) != N){
    node.names <- V(network)$name
  }

  #make adjacency matrix
  adj.mat <- as.matrix(as_adjacency_matrix(network, type = "both"))


  #get bootstrap samples
  if(is.null(quantile_n)){quantile_n <- 0}

  local_boot_res = localboot(adj.mat,B,
                             returns = "boot",
                             quantile_n = quantile_n,
                             weighted = weighted,
                             fast = fast,
                             dist_func = dist.func)

  # Change the name of the nodes and make them igraph objects
  boot.result <- vector(mode = "list", length = B)
  node.names.dat <- data.frame(name = node.names,
                           v = paste0("v", 0:(N-1)),
                           v.r = paste("v", 1:N)) # they name things from 0 to N-1 becuase they use C++


  for(i in 1:B){
    #get the index in c++
    sampled.v.boot  <- rownames(local_boot_res[[i]])
    #get the index in R
    orig.node.numbers <- as.numeric(substring(sampled.v.boot , 2, nchar(sampled.v.boot ))) + 1
    #get the name of the node in the original nework
    sampled.nodes.boot <-  node.names.dat$name[orig.node.numbers]

    #make each node in the bootstrap sample have its own name so igraph identifies them individually
    rownames(local_boot_res[[i]]) <-  paste0("vb", 1:N)
    colnames(local_boot_res[[i]]) <- paste0("vb", 1:N)

    #make the bootstrap sample an igraph object then add in node information
    boot.result[[i]] <- graph_from_adjacency_matrix(local_boot_res[[i]], mode = "undirected")
    V(boot.result[[i]])$original.index.cpp <- sampled.v.boot # c++ node index
    V(boot.result[[i]])$original.index.r <- paste0("v", orig.node.numbers) # R node index
    V(boot.result[[i]])$boot.index <-  paste0("vb", 1:N) # index in the bootstrap sample
    V(boot.result[[i]])$name <- sampled.nodes.boot # name of the sampled node from the original sample

  }

  return(boot.result)


}
