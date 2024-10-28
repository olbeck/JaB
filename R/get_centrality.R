#' Calculate give centrality statistic on a network
#'
#' @param func.name A character string of the function name used to calculate the desired centrality statistic.
#' @param graph The graph object. This should be the appropriate object type needed for the `func.name` specified.
#' @param package.name A character string of the name of the package the `func.name` function is in. If left as `NULL` if the function will be called as loaded in the users environment.
#' @param func.args A list of additional arguments the `func.name` function may need beyond the `graph` object.
#' @returns A numeric vector of the centrality statistic as calculated by `func.name`.
#' @examples
#' #COME BACK TO THESE EXAMPLES ONCE DATA IS WRITTEN
#' #get_centrality("closeness", paul.igraph, package.name = "igraph, func.args = list(normalized = T))
#' #get_centrality("degree", paul.igraph, package.name = "igraph, func.args = list(normalized = T))
#' @export

get_centrality <- function(func.name,
                           graph,
                           package.name = NULL,
                           func.args = NULL){

  #if there is a give package name, get the function from that package
  #else get the function as loaded in the current enviornment
  if(!is.null(package.name)){
    func <- getExportedValue(package.name, func.name)
  }else(
    func <- func.name
  )

  #if the func.args is null then the only argument in do.call is the graph itself
  #otherwise add graph to args for do.call
  if(!is.null(func.args)){
    func.args[["graph"]] <- graph
  }else{
    func.args <- list(graph = graph)
  }

  #call the function
  center_stat <- do.call(
    what = func,
    args = func.args
  )

  # there are a few functions that output extra information - e.g. igraph::eigen_centrality
  # if this is the case, only return the list elements that are a vector of length the number of nodes in the network
  # else, return the vector
  if(!is.atomic(center_stat)){
    vec.lengths <- sapply(center_stat, function(x){if (is.vector(x)) length(x) else NA})
    id <- which(vec.lengths == igraph::gorder(graph))
    return(center_stat[[id]])
  }else(
    return(center_stat)
  )
}
