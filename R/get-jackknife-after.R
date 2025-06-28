#' Jackknife-After in JaB Algorithm for Networks
#'
#' This function performs the "Jackknife-after" portion of the Jackknife-after-Bootstrap (JaB) algorithm for networks.
#'
#' @param network An igraph object representing the original network.
#' @param boot.result A list of bootstrap network samples, as produced by a `bootstrap_` function.
#' @param quant A numeric value specifying the upper quantile used to flag influential nodes (e.g., `0.95`).
#' @param func.name A character string specifying the centrality function to be applied (e.g., `"degree"`, `"betweenness"`). See \code{\link{get_centrality}} for details.
#' @param package.name (Optional) A character string specifying the package containing `func.name`. See \code{\link{get_centrality}} for details.
#' @param func.args (Optional) A list of additional arguments for `func.name`. See \code{\link{get_centrality}} for details.
#' @param nodes (Optional) A vector of node names to run JaB algorithm for. If `NULL`, algorithm will be run on all \eqn{n} nodes in `network`.
#'
#' @return A data frame containing:
#' \itemize{
#'   \item \code{Node_Number}: Numeric IDs of nodes.
#'   \item \code{Node_Name}: Name of nodes.
#'   \item \code{Orig_Stat}: The original centrality statistic of each node.
#'   \item \code{Boot_mean}, \code{boot_sd}, \code{boot_skew}: The mean, standard deviaiton, and skewness of the elements of \eqn{\Gamma_i}
#'   \item \code{Upper_Quantile}: Upper quantile of the jackknife-after-bootstrap distribution of centrality statistic for each node.
#'   \item \code{Influential}: Logical indicating if each node is influential, i.e. is `Orig_Stat` greater than `Upper_Quantile`?
#'   \item \code{Rank}: Rank from most (1) to least (\eqn{n}) influential. There can be ties in the rankings.
#'   \item \code{Can_Jackknife}: Logical indicating if there were bootstrap samples that
#'  *did not* include that node, meaning there *are* jackknife-after sample to generate
#'  the distribution of centrality statistics in networks that do not contain that node.
#'  If `FALSE`, then all bootstrap samples contained that node and the `Upper_Quantile` column will generally be `NA`.
#'  If many nodes are `FALSE` it could mean that the bootstrap method is poorly tuned and is
#'  sampling more nodes that is appropriate for this data set.
#'  If only a few nodes are `FALSE` it could mean the bootstrap method is
#'  poorly tuned for this data set, or it could mean that the node is extremely influential as
#'  it is highly improbable to generate a bootstrap sample that does not contain that node.
#'  Which explanation is appropriate depends on the data set and the bootstrap method used.
#'  \item `Num_Boot_Samps`: Number of bootstrap samples used to construct the distribution in the Jackknife-after step.
#'  If there are \eqn{B} bootstrap samples in `boot.result`, then \eqn{B - Num_Boot_Samps}
#'  bootstrap samples *contained* node \eqn{v_i} and, \eqn{Num_Boot_Samps} *did not contain* node \eqn{v_i}
#'  If `Can_Jackknife` is `FALSE`, then this number will be 0 (i.e. *all* bootstrap samples contained
#'  node \eqn{v_i} and thus *none* of them can be used in the Jackknife-after step).
#' }
#'
#' @details
#' This function performs the "jackknife-after" portion of the JaB algorithm for
#' network data. See [jab_network()] wrapper function for entire algorithm.
#'
#' @seealso [jab_network()]
#'
#' @examples
#' library(igraphdata)
#' library(igraph)
#' data(karate)
#'
#' #first get bootstrap samples
#' boot.result <- bootstrap_snowboot(karate, B = 100, num.seed = 1, num.wave = 2)
#'
#' #then do jackknife-after
#' get_jackknife_after(network = karate,
#'   boot.result = boot.result,
#'   quant = 0.9,
#'   func.name = "degree",
#'   package.name = "igraph",
#'   func.args = list(normalized = TRUE),
#'   nodes = NULL)
#'
#' @importFrom igraph V gorder
#' @importFrom dplyr group_by arrange ungroup mutate select min_rank desc
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
  central.result <-
    get_bootstrap_centrality(boot.result = boot.result,
                             func.name = func.name,
                             package.name = package.name,
                             func.args = func.args)




  #node list in each bootstrap sample
  m <- length(ids)
  node.list <- lapply(boot.result, get_nodes)
  number.boot.samps <- rep(NA, m)
  jack.result <- rep(NA, m)
  can.jackknife <- rep(TRUE,m)

  boot.mean <- rep(NA, m)
  boot.sd <- rep(NA, m)
  boot.skew <- rep(NA, m)

  #centrality_storage <- list(n) #used for exploration purposes, not actually needed in algorithm
  counter <- 0
  for(i in ids){
    counter <- counter+1
    #get index of all bootstrap samples that do not have node i
    keep_index <- unlist(lapply(node.list, function(x){all(names[i] !=x)}))

    #get number of bootstrap samples node i appeared in
    number.boot.samps[i] <- sum(keep_index)

    #if there are no bootstrap samples without node i, break
    if(all(!keep_index)){
      can.jackknife[counter] <- FALSE
      next
    }

    #Get centrality statistic for all networks in keep_index
    #This repeats each vertex each number of times it was samples
    # i.e. if in bootstrap sample b, node i was sampled 3 times, there will be 3 repetitions of the i^th statistics in the null distribution
    centrality_vector <- stats::na.omit(c(unlist(central.result[keep_index])))
    has_i <- stats::na.omit(c(unlist(central.result[!keep_index])))

    #just for exploring null distribution shape, not actually needed to run the algorithm
    #this generally uses a lot of storage even for moderately large B
    #centrality_storage[[i]] <- centrality_vector

    #Get Quantiles
    jack.result[counter] <- stats::quantile(centrality_vector, quant, na.rm = T)

    #Get means and sd
    boot.mean[i] <- mean(has_i, na.rm = TRUE)
    boot.sd[i] <- sd(has_i, na.rm = TRUE)
    boot.skew[i] <- n / (n-1) / (n-2) * sum((has_i -boot.mean[i])^3 ) / boot.sd[i]^3

  }


  ### Return something useful
  ret <- data.frame(Node_Number = ids,
                    Node_Name = names[ids],
                    Orig_Stat = orig.stat[ids],
                    Boot_mean = boot.mean[ids],
                    Boot_sd = boot.sd[ids],
                    Boot_skew = boot.skew[ids],
                    Upper_Quantile = jack.result,
                    Can_Jackknife = can.jackknife,
                    Num_Boot_Samps = number.boot.samps)

  #order from most outside on CI to least
  ret$top <- ! (ret$Orig_Stat > ret$Upper_Quantile )
  ret$diff <- ret$Orig_Stat - ret$Upper_Quantile
  ret <- ret %>%
    dplyr::group_by(top) %>%
    dplyr::arrange(dplyr::desc(diff)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Rank = dplyr::min_rank(dplyr::desc(diff))) %>%
    dplyr::mutate(Influential = !top)%>%
    dplyr::select(-c( top, diff)) %>%
    dplyr::select(Node_Number, Node_Name, Orig_Stat, #original sample information
                  Boot_mean, Boot_sd, Boot_skew, #bootstrap information of \Gamma_i
                  Upper_Quantile, Influential, Rank, #JaB information from \Gamma_{-i}
                  Can_Jackknife, Num_Boot_Samps) #Bootstrap Information


  return(ret)
}
