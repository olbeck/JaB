#' Convert an Object to an igraph Graph
#'
#' Converts an object of specified type (e.g., adjacency matrix, edgelist) to an igraph graph object.
#' Supported input types include `igraph`, `matrix`, `sparsematrix`, and `edgelist`.
#'
#' @param obj Object output from `bootstrap_` functions. Must be of type `igraph`, `matrix`, `sparsematrix`, or `edgelist`.
#' @param obj.type A character string specifying the type of `obj`. Options include `"igraph"`, `"matrix"`, `"sparsematrix"`, or `"edgelist"`. Same as `output.type` parameter in `bootstrap_` function.
#' @param mode A character string specifying whether the graph is `directed`, `undirected`, `max`, `min`, `upper`, `lower`, `plus`).
#' Defaults to `undirected`. See `igraph::graph_from_adjacency_matrix` for details.
#' @param weighted A logical indicating if the graph is weighted. If `NULL`, no weights are added. Defaults to `NULL`.
#'
#' @return An igraph object representing the graph.
#'
#' @examples
#' # For an adjacency matrix
#' #adj_matrix <- matrix(c(0, 1, 1, 0), nrow = 2)
#' #make_igraph(adj_matrix, obj.type = "matrix")
#'
#' # For an edge list
#' #edge_list <- matrix(c(1, 2, 2, 3), ncol = 2)
#' #make_igraph(edge_list, obj.type = "edgelist", mode = "directed")
#'
#' @importFrom igraph graph_from_adjacency_matrix graph_from_edgelist
#' @export

make_igraph <- function(obj, obj.type, mode = "undirected", weighted = NULL){

  if(obj.type == "igraph"){
    return(obj)
  }else if(obj.type %in% c("matrix", "sparsematrix")){
    return(
      igraph::graph_from_adjacency_matrix(
        obj, mode = mode, weighted = weighted))
  }else if(obj.type == "edgelist"){
    obj<-matrix(obj, ncol=2)
    return(
      igraph::graph_from_edgelist(
        obj, directed = ifelse(mode == "undirected", FALSE, TRUE)))
  }else{
    stop("Object is not of a supported class.")
  }
}

#' Compute Bootstrap Centrality Statistics from Bootstrap Samples
#'
#' This function takes bootstrap samples of graph data from `bootstrap_` functions and computes specified centrality statistics for each sample.
#'
#' @param boot.result A list of bootstrap samples, where each sample represents a graph in a specified format (e.g., adjacency matrix, edgelist). This is output from the `bootstrap_` function.
#' @param obj.type A character string specifying the type of `obj` in each bootstrap sample. Options include `"igraph"`, `"matrix"`, `"sparsematrix"`, or `"edgelist"`. This is the `output.type` of the `bootstrap_` function.
#' @param func.name A character string specifying the centrality function to apply (e.g., `"degree"`, `"betweenness"`). See \code{\link{get_centrality}} for details.
#' @param package.name An optional character string indicating the package name containing the centrality function. Defaults to `NULL` (assuming `func.name` is available). See \code{\link{get_centrality}} for details.
#' @param func.args An optional list of additional arguments to pass to the centrality function. Defaults to `NULL`. See \code{\link{get_centrality}} for details.
#'
#' @return A list of length `B`, where each element is the bootstrap centrality statistic result from applying the specified centrality statistic function.
#'
#' @details Calculates bootstrap centrality statistics from give bootstrap samples and a specified centrality statistic.
#'
#' @examples
#' # Assuming `boot_samples` is a list of adjacency matrices from bootstrap samples
#' #get_bootstrap_centrality(boot_samples, obj.type = "matrix", func.name = "degree")
#'
#' @seealso \code{\link{make_igraph}}, \code{\link{get_centrality}}
#' @export
get_bootstrap_centrality <- function(boot.result, obj.type,
                                     func.name,
                                     package.name = NULL,
                                     func.args = NULL){

  B <- length(boot.result)
  ret <- vector(mode = "list", length = B)
  for(b in 1:B){
    obj <- boot.result[[b]]
    g <- make_igraph(obj, obj.type )
    ret[[b]] <- get_centrality(func.name, g, package.name = NULL, func.args = NULL)
  }

  return(ret)
}
