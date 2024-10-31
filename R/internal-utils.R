#' Convert Network Objects to New Type
#'
#' Converts an input list of networks between various representations: `matrix`, `dgCMatrix`, `edgelist`, and `igraph`.
#'
#' @param obj A list of network objects, which must match the `input.type` specified.
#' @param input.type A character string specifying the input type of `obj`. Options include `"igraph"`, `"matrix"`, `"dgCMatrix"`, or `"edgelist"`.
#' @param output.type A character string specifying the desired output type. Options are `"igraph"`, `"matrix"`, `"dgCMatrix"`, or `"edgelist"`.
#'
#' @return A converted list of network objects in the specified `output.type` format.
#'
#' @details
#' The function converts each network in `obj` to the specified `output.type`. Conversion logic is applied based on `input.type` and `output.type`.
#' - If `input.type` and `output.type` are the same, `obj` is returned as is.
#' - Conversions from adjacency matrices to igraph objects are handled via `igraph::graph_from_adjacency_matrix`.
#' - Conversions from igraph objects to edge lists are handled with `igraph::as_edgelist`.
#' - Conversions from igraph objects to adjacency matrices are handled with `igraph::as_adjacency_matrix`.
#'
#' To use `"dgCMatrix"` for `input.type` or `output.type`, the package `Matrix` must be installed.
#'
#' @examples
#' # Convert list of adjacency matrices to igraph objects
#' adj_matrices <- replicate(3, matrix(c(0, 1, 1, 0), nrow = 2), simplify = FALSE)
#' make_network_type(adj_matrices, input.type = "matrix", output.type = "igraph")
#'
#' # Convert list of igraph objects to sparse adjacency matrices
#' igraph_list <- lapply(adj_matrices, igraph::graph_from_adjacency_matrix)
#' make_network_type(igraph_list, input.type = "igraph", output.type = "dgCMatrix")
#'
#' @importFrom igraph graph_from_adjacency_matrix as_adjacency_matrix as_edgelist
#' @importFrom Matrix Matrix
#' @export
make_network_type <- function(obj, input.type, output.type){

  #change "dgCMatrix" to "dgCMatrix"
  if(input.type == "dgCMatrix"){input.type <- "dgCMatrix"}
  if(output.type == "dgCMatrix"){output.type <- "dgCMatrix"}


  if(input.type == output.type){
    return(obj)
  }else if(input.type %in% c("matrix", "dgCMatrix")){
    g.list <- base::lapply(obj, function(x){
      igraph::graph_from_adjacency_matrix(x, mode = "undirected",
                                          weighted = NULL, diag = F)})
    if(output.type == "igraph"){
      return(g.list)
    }else if(output.type == "edgelist"){
      return(base::lapply(g.list,igraph::as_edgelist))
    }else if(input.type == "matrix" & output.type == "dgCMatrix"){
      return(base::lapply(obj, function(x){Matrix::Matrix(x, sparse = T)} ))
    }else if(input.type == "dgCMatrix" & output.type == "matrix"){
      return(base::lapply(obj, function(x){Matrix::Matrix(x, sparse = F)} ))
    }
  }else if(input.type == "igraph"){
    if(output.type %in% c("matrix", "dgCMatrix")){
      is.sparse <- output.type == "dgCMatrix"
      return(
        lapply(obj,
               function(x){igraph::as_adjacency_matrix(x, type = "both", sparse = is.sparse)}))
    }else if(output.type == "edgelist"){
      return(base::lapply(obj,igraph::as_edgelist))
    }
  }else if(input.type == "edgelist"){
    g.list <- base::lapply(obj, function(x){
      igraph::graph_from_edgelist(as.matrix(x, ncol =2), directed = F)})
    if(output.type == "igraph"){
      return(g.list)
    }else if(output.type %in% c("matrix", "dgCMatrix")){
      is.sparse <- output.type == "dgCMatrix"
      return(
        lapply(g.list,
               function(x){igraph::as_adjacency_matrix(x, type = "both", sparse = is.sparse)}))
    }
  }

}

#' Detect the Type of Objects in a List
#'
#' Attempts to detect the type of objects in a list based on their class and dimensions, as used in the JaB package. This function checks if all elements in the list are of the same class and determines their type, such as `"igraph"`, `"dgCMatrix"`, `"edgelist"`, or `"matrix"`.
#'
#' @param obj A list of objects to be analyzed. All objects in the list must belong to the same class.
#'
#' @return A character string indicating the type of objects in the list. Possible return values are:
#' \itemize{
#'   \item `"igraph"`: If the objects are igraph objects.
#'   \item `"dgCMatrix"`: If the objects are sparse matrices of class `dgCMatrix`.
#'   \item `"edgelist"`: If the objects are matrices with two columns, representing an edge list.
#'   \item `"matrix"`: If the objects are square matrices, typically representing adjacency matrices.
#' }
#'
#' @details
#' The function first verifies that all objects in the list are of the same class. It then checks if the class matches known types (`"igraph"` or `"dgCMatrix"`). If the class is not one of these, it further examines the dimensions of each object to determine if they represent an edge list (two-column matrix) or an adjacency matrix (square matrix).
detect_type <- function(obj){

  #input obj = list of objects
  #attempts to detect type of object as related to JaB package

  classes <- unique(unlist(lapply(obj, class)))
  if("igraph" %in% classes){
    return("igraph")
  }else if( "dgCMatrix" %in% classes){
    return("dgCMatrix")
  }else{
    dims <- do.call(rbind, lapply(obj, dim))
    is.edgelist <- all(dims[,2] == 2)
    is.adj <- all(dims[, 1] == dims[, 2])
    if(is.edgelist & !is.adj){return("edgelist")} #2-by-2 matrix defaults to adjacency matrix.
    if(is.adj){return("matrix")}
  }
  stop("Object is not of a recognizable class")

}



#' Node Names from a Network Object
#'
#' Extracts the names of nodes (vertices) from a network object.
#' This is for internal use for network types in the JaB package
#'
#' @param network A network object with \eqn{n}, which may be of types `matrix`, `dgCMatrix` `edgelist`, or `igraph`).
#'
#' @return A vector of node names. If node names are not assigned, it returns a sequence of integers from 1 to the number of nodes.
#' @seealso \code{\link{detect_type}}, \code{\link{make_network_type}}
#' @importFrom igraph V gorder
get_nodes <- function(network){
  class <- JaB:::detect_type(list(network))
  network <- make_network_type(list(network), class, "igraph")[[1]]
  names <- igraph::V(network)$name
  if(is.null(names)){names <- 1:igraph::gorder(network)}
  return(names)
}

