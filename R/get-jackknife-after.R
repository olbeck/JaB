#' Jackknife After-Bootstrap
#'
#' Detects influential nodes in a network with the jackknife-after-bootstrap algorithm.
#'
#' @param network An igraph object representing the original network.
#' @param boot.result A list of bootstrap network samples, as produced by a `bootstrap_` function.
#' @param quant A numeric value of length 2 specifying the upper quantile used to flag influential nodes (e.g., \code{ 0.95}).
#' @param func.name A character string specifying the centrality function to be applied (e.g., `"degree"`, `"betweenness"`). See \code{\link{get_centrality}} for details.
#' @param package.name (Optional) A character string specifying the package containing \code{func.name}. See \code{\link{get_centrality}} for details.
#' @param func.args (Optional) A list of additional arguments for \code{func.name}.See \code{\link{get_centrality}} for details.
#' @param nodes (Optional) A vector of node names to run JaB algorithm for. If `NULL`, algorithm will be run on all \eqn{n} nodes.
#'
#' @return A data frame containing:
#' \itemize{
#'   \item \code{Node_Number}: Numeric IDs of nodes.
#'   \item \code{Node_Name}: Name of nodes.
#'   \item \code{Orig_Stat}: The original centrality statistic of each node.
#'   \item \code{Upper_Quantile}: Upper quantile of the jackknife-after-bootstrap distribution of centrality statistic for each node.
#'   \item \code{Influential}: Logical indicating if each node is influential, i.e. is `Orig_Stat` greater than `Upper_Quantile`?
#'   \item \code{Influential}: Logical indicating if there were bootstrap samples that
#'  *did not* include that node, meaning there *are* jackknife-after sample to generate
#'  the distribution of centrality statistics in networks that do not contain that node.
#'  If `FALSE`, then all bootstrap samples contained that node and the `Upper_Quantile` column will generally be `NA`.
#'  If many nodes are `FALSE` it
#'  could mean that the bootstrap method is poorly tuned and is sampling more nodes that is appropriate
#'  for this data set. If only a few nodes are `FALSE` it could mean the bootstrap method is
#'  poorly tuned for this data set, or it could mean that the node is extremely influential as
#'  it is highly improbable to generate a bootstrap sample that does not contain that node.
#'  Which explanation is appropriate depends on the data set and the bootstrap method used.
#' }
#'
#' @details
#' This function performs a jackknife after-bootstrap procedure for each node in the network.
#' OUTLINE ALGORITHM
#'
#' @examples
#' #add example later
#'
#' @importFrom igraph V gorder
#' @importFrom dplyr group_by arrange ungroup mutate select
#' @export
get_jackknife_after <- function(network,
                          boot.result,
                          quant,
                          func.name,
                          package.name = NULL,
                          func.args = NULL,
                          nodes = NULL){

  n <- igraph::gorder(network)
  names <- get_nodes(network)
  orig.stat <- get_centrality(network, func.name, package.name , func.args)

  if(is.null(nodes)){
    nodes <- names
    ids <- 1:n
  }else{
    ids <- which(names %in% nodes)
  }

  #Get bootstrap centrality statistics
  obj.type <- detect_type(boot.result)
  central.result <- get_bootstrap_centrality(boot.result, func.name, obj.type = obj.type, func.args = func.args)


  #node list in each bootstrap sample
  m <- length(ids)
  node.list <- lapply(boot.result, get_nodes)
  jack.result <- rep(NA, m)
  can.jackknife <- rep(TRUE,m)

  #centrality_storage <- list(n) #used for exploration purposes, not actually needed in algorithm
  counter <- 0
  for(i in ids){
    counter <- counter+1
    #get index of all bootstrap samples that do not have node i
    keep_index <- unlist(lapply(node.list, function(x){all(names[i] !=x)}))


    #if there are no bootstrap samples without node i, break
    if(all(!keep_index)){
      can.jackknife[counter] <- FALSE
      next
    }

    #Get centrality statistic for all networks in keep_index
    #This repeats each vertex each number of times it was samples
    # i.e. if in bootstrap sample b, node i was sampled 3 times, there will be 3 repetitions of the i^th statistics in the null distribution
    centrality_vector <- c(unlist(central.result[keep_index]))

    #just for exploring null distribution shape, not actually needed to run the algorithm
    #this generally uses a lot of storage even for moderately large B
    #centrality_storage[[i]] <- centrality_vector

    #Get Quantiles
    jack.result[counter] <- quantile(centrality_vector, quant, na.rm = T)


  }


  ### Return something useful
  ret <- data.frame(Node_Number = ids,
                    Node_Name = names[ids],
                    Orig_Stat = orig.stat[ids],
                    Upper_Quantile = jack.result,
                    Can_Jackknife = can.jackknife)

  #order from most outside on CI to least
  ret$top <- ! (ret$Orig_Stat > ret$Upper_Quantile )
  ret$diff <- abs(ret$Orig_Stat - ret$Upper_Quantile)
  ret <- ret %>%
    dplyr::group_by(top) %>%
    dplyr::arrange(desc(diff), .by_group = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Influential = !top)%>%
    dplyr::select(-c( diff, top))


  return(ret)
}
