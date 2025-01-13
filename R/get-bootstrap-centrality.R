#' Compute Bootstrap Centrality Statistics from Bootstrap Samples
#'
#' This function takes bootstrap samples of graph data from `bootstrap_` functions and computes specified centrality statistics for each sample.
#'
#' @param boot.result A list of bootstrap samples, where each sample represents a graph in a specified format (e.g., adjacency matrix, edgelist). This is output from the `bootstrap_` function.
#' @param func.name A character string specifying the centrality function to apply (e.g., `"degree"`, `"betweenness"`). See \code{\link{get_centrality}} for details.
#' @param package.name An optional character string indicating the package name containing the centrality function. Defaults to `NULL` (assuming `func.name` is available). See \code{\link{get_centrality}} for details.
#' @param func.args An optional list of additional arguments to pass to the centrality function. Defaults to `NULL`. See \code{\link{get_centrality}} for details.
#' @param obj.type A character string specifying the type of `obj` in each bootstrap sample. Options include `"igraph"`, `"matrix"`, `"dgCMatrix"`, or `"edgelist"`. This is the `output.type` of the `bootstrap_` function.
#' @return A list of length `B`, where each element is the bootstrap centrality statistic result from applying the specified centrality statistic function.
#'
#' @details Calculates bootstrap centrality statistics from give bootstrap samples and a specified centrality statistic.
#'
#' @examples
#' library(igraph)
#' data("karate")
#' set.seed(89)
#' boot.result <- bootstrap_latent_space(
#'   karate, d = 2, B = 10)
#'
#' #Calculate degree centrality (from igraph)
#' central.result <- get_bootstrap_centrality(boot.result,
#'                                            func.name = "degree",
#'                                            package.name = "igraph",
#'                                            func.args = list(normalized = TRUE))
#' central.result[[1]]
#'
#' #plot bootstrap centralities vs original centrality
#' hist(unlist(central.result), freq = FALSE,
#'      xlab = "degree centrality", main = "Bootstrap Degree Centralities")
#' lines(density(igraph::degree(karate, normalized = TRUE)), col = "red")
#'
#'
#' @seealso \code{\link{make_network_type}}, \code{\link{get_centrality}}
#' @export
get_bootstrap_centrality <- function(boot.result,
                                     func.name,
                                     package.name = NULL,
                                     func.args = NULL,
                                     obj.type=NULL){

  if(is.null(obj.type)){obj.type <- detect_type(boot.result)}

  B <- length(boot.result)
  ret <- vector(mode = "list", length = B)
  for(b in 1:B){
    obj <- boot.result[[b]]
    g <- make_network_type(list(obj), obj.type, output.type = "igraph" )
    ret[[b]] <- get_centrality(g[[1]], func.name, package.name = package.name, func.args = func.args)
  }

  return(ret)
}
