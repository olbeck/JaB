#' Find Largest Component of a Network
#'
#' This function takes in an `igraph` network object and returns and `igraph` object
#' of the largest connected component.
#'
#' @param network An `igraph` object.
#' @return An `igraph` object which is the largest connected component of the network.
#' If the network is already connected, the function returns the input network unchanged.
#' @examples
#' library(igraph)
#' #unconnected network
#' data("hamster")
#' is.connected(hamster)
#' get_largest_component(hamster)
#'
#' #connected network
#' data("karate")
#' is.connected(karate)
#' get_largest_component(karate)
#'
get_largest_component <- function(network){

  cl <- class(network)
  if(cl != "igraph"){
    stop("network must be an igraph object")
  }

  is.con <- igraph::is.connected(network)
  if(is.con){
    return(network)
  }else{
    com <- igraph::components(network)
    com.id <- which.max(com$csize)
    keep.nodes <- com$membership == com.id
    return(igraph::subgraph(network, com.id))
  }

}
