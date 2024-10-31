#' Jackknife After-Bootstrap
#'
#' Detects influential nodes in a network with the jackknife-after-bootstrap algorithm.
#'
#' @param network An igraph object representing the original network.
#' @param boot.result A list of bootstrap network samples, as produced by a `bootstrap_` function.
#' @param quant A numeric value of length 2 specifying the upper quantile used to flag influential nodes (e.g., \code{ 0.95}).
#' @param func.name A character string specifying the centrality function to be applied (e.g., `"degree"`, `"betweenness"`). See \code{\link{get_centrality}} for details
#' @param package.name (Optional) A character string specifying the package containing \code{func.name}. See \code{\link{get_centrality}} for details
#' @param func.args (Optional) A list of additional arguments for \code{func.name}.See \code{\link{get_centrality}} for details
#'
#' @return A data frame containing:
#' \itemize{
#'   \item \code{Node_Number}: Numeric IDs of nodes.
#'   \item \code{Node_Name}: Name of nodes.
#'   \item \code{Orig_Stat}: The original centrality statistic of each node.
#'   \item \code{Upper_Quantile}: Upper quantile of the jackknife-after-bootstrap distribution of centrality statistic for each node.
#'   \item \code{Influential}: Logical indicating if each node is influential, i.e. is `Orig_Stat` greater than `Upper_Quantile`?
#' }
#'
#' @details
#' This function performs a jackknife after-bootstrap procedure for each node in the network. It calculates the original centrality statistic for each node, then examines the bootstrap samples, excluding each node to generate a null distribution for that node’s centrality. Based on this distribution, it computes specified quantiles (default typically 0.025 and 0.975) to create confidence intervals.
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
                          func.args = NULL){

  n <- igraph::gorder(network)
  names <- JaB:::get_nodes(network)
  orig.stat <- get_centrality(network, func.name, package.name , func.args)



  #Get bootstrap centrality statistics
  obj.type <- JaB:::detect_type(boot.result)
  central.result <- get_bootstrap_centrality(boot.result, func.name, obj.type = obj.type)


  #node list in each bootstrap sample
  node.list <- lapply(boot.result, JaB:::get_nodes)
  jack.result <- matrix(NA, nrow = n, ncol = 1)

  #centrality_storage <- list(n) #used for exploration purposes, not actually needed in algorithm

  for(i in 1:n){
    #get index of all bootstrap samples that do not have node i
    keep_index <- unlist(lapply(node.list, function(x){all(names[i] !=x)}))

    #Get centrality statistic for all networks in keep_index
    #This repeats each vertex each number of times it was samples
    # i.e. if in bootstrap sample b, node i was sampled 3 times, there will be 3 repetitions of the i^th statistics in the null distribution
    centrality_vector <- c(unlist(central.result[keep_index]))

    #just for exploring null distribution shape, not actually needed to run the algorithm
    #this generally uses a lot of storage even for moderately large B
    #centrality_storage[[i]] <- centrality_vector

    #Get Quantiles
    jack.result[i, ] <- quantile(centrality_vector, quant, na.rm = T)

  }


  ### Return something useful
  ret <- data.frame(Node_Number = 1:n,
                    Node_Name = names[1:n],
                    Orig_Stat = orig.stat,
                    Upper_Quantile = jack.result[, 1])

  #order from most outside on CI to least
  ret$top <- ! (ret$Orig_Stat > ret$Upper | ret$Orig_Stat < ret$Lower)
  ret$diff <- abs(ret$Orig_Stat - ret$Upper)
  ret <- ret %>%
    dplyr::group_by(top) %>%
    dplyr::arrange(desc(diff), .by_group = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Influential = !top)%>%
    dplyr::select(-c( diff, top))


  return(ret)
}
