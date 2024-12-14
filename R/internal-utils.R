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
#' Networks with \eqn{n} nodes can be represented as,
#' \itemize{
#'    \item{"igraph": }{an igraph object}
#'    \item{"matrix": }{an \eqn{n}-by-\eqn{n} adjacency matrix, class `"matrix"`}
#'    \item{"dgCMatrix": }{an \eqn{n}-by-\eqn{n} adjacency matrix as a sparse matrix, class `{dgCMatrix}`, see \link[Matrix]{Matrix} package for details}
#'    \item{"edgelist": }{an \eqn{\binom{n}{2}}-by-2 matrix where each row is the pair of nodes an edge is attached to}
#' }
#' To use `"dgCMatrix"` for `input.type` or `output.type`, the package `Matrix` must be installed.
#' @seealso [detect_type()]
#'
#' @examples
#' # Convert list of adjacency matrices to igraph objects
#' one_matrix <- matrix(c(0, 1, 1, 1, 0, 0, 1, 0, 0), nrow = 3, byrow = TRUE)
#' matrix_list <- replicate(3, one_matrix, simplify = FALSE)
#' igraph_list <-
#'   make_network_type(matrix_list,
#'                    input.type = "matrix",
#'                    output.type = "igraph")
#'
#' # Same thing now using detect type
#' type <- detect_type(matrix_list)
#' type
#' igraph_list <-
#'   make_network_type(matrix_list,
#'                    input.type = type,
#'                    output.type = "igraph")
#'
#' # Convert list of igraph objects to sparse matrix
#' sparsematrix_list <-
#'   make_network_type(igraph_list,
#'                     input.type = "igraph",
#'                     output.type = "dgCMatrix")
#' detect_type(sparsematrix_list)
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
#' Attempts to detect the type of network representation in a list.  Networks can be represented as `"igraph"`, `"dgCMatrix"`, `"edgelist"`, or `"matrix"`.
#'
#' @param obj A list of networks. All networks in the list must have the same representation. e.g. all networks in the list must be an edge list.
#'
#' @return A character string indicating the representation of the networks in the list. Possible return values are:
#' \itemize{
#'   \item `"igraph"`: If the networks are igraph objects.
#'   \item `"dgCMatrix"`: If the networks are sparse matrices of class `dgCMatrix`.
#'   \item `"edgelist"`: If the networks are matrices with two columns, representing an edge list.
#'   \item `"matrix"`: If the networks are square matrices, typically representing adjacency matrices.
#' }
#'
#' @details
#' The function detects if the input list of networks are represented as
#' `"igraph"`, `"dgCMatrix"`, `"edgelist"` or `"matrix"`. This function is primarily
#' used internally for verifying the networks are in the correct format for the
#' intended operation being used.
#'
#' 2-by-2 matrices default to `"matrix"`.
#'
#' @inherit make_network_type examples
#' @seealso [make_network_type()]
#' @export
detect_type <- function(obj){

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
#' @param network A network object with \eqn{n} nodes. Network can be represented as a `matrix`, `dgCMatrix` `edgelist`, or `igraph`). See \code{\link{make_network_type}} for details.
#'
#' @return A vector of node names. If node names are not assigned, it returns a sequence of integers from 1 to \eqn{n}.
#' @seealso \code{\link{detect_type}}, \code{\link{make_network_type}}
#' @importFrom igraph V gorder
#' @examples
#' data(paul.revere)
#' get_nodes(paul.revere)
#' @export
get_nodes <- function(network){
  class <- detect_type(list(network))
  network <- make_network_type(list(network), class, "igraph")[[1]]
  names <- igraph::V(network)$name
  if(is.null(names)){names <- 1:igraph::gorder(network)}
  return(names)
}

