#' JaB Algorithm for Network Data
#'
#' Wrapper function for the Jackknife-after-Bootstrap (JaB) algorithm for networks.
#'
#' @param network The network as an `igraph` object.
#' @param central.func.name A character string of the function name used to calculate the desired centrality statistic.
#' @param central.package.name (Optional) A character string of the name of the package that the `central.func.name` function is in. If left as `NULL` if the function will be called as loaded in the users environment.
#' @param central.func.args (Optional) A list of additional arguments the `central.func.name` function may need beyond the `network` object.
#' @param bootstrap.func.name A character string of the `JaB::boostrap_` function name used to generate bootstrap samples. (e.g. [bootstrap_snowboot()])
#' @param bootstrap.package.name (Optional) A character string of the name of the package that the `bootstrap.func.name` function is in. If left as `NULL` if the function will be called as loaded in the users environment.
#' @param bootstrap.func.args (Optional) A list of additional arguments the `bootstrap.func.name` function may need beyond the `network` object.
#' @param B Number of bootstrap samples. Default is 1,000.
#' @param quant A numeric value specifying the upper quantile used to flag influential nodes (e.g., `0.95`).
#' @param nodes (Optional) A vector of node names to run JaB algorithm for. If `NULL`, algorithm will be run on all \eqn{n} nodes in `network`.
#' @param return.boot.samples Logical, should list of bootstrap samples used in algorithm be returned? Default is `FALSE`.
#'
#' @return If `return.boot.samples` is `FALSE`, returns a data frame containing:
#' \itemize{
#'   \item \code{Node_Number}: Numeric IDs of nodes.
#'   \item \code{Node_Name}: Name of nodes.
#'   \item \code{Orig_Stat}: The original centrality statistic of each node.
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
#'  \item `Num_Boot_Samps`: Number of bootstrap samples the node appeared in. If there are
#'  \eqn{B} bootstrap samples in `boot.result`, then \eqn{B} - `Num_Boot_Samps` bootstrap samples are
#'  used to calculate `Upper_Quantile`. If `Can_Jackknife` is `FALSE`, then this number
#'  will be 0.
#' }
#'
#' If `return.boot.samples` is `TRUE`, returns a list containing,
#' \enumerate{
#'   \item \code{bootstrap}: List of `B` bootstrap samples.
#'   \item \code{jack.after}: Results of the JaB algorithm as the data frame listed above.
#' }
#'
#' @details
#'
#' Suppose we have a network \eqn{G} with \eqn{n} nodes. Specify a centrality statistic \eqn{\gamma}.
#'
#' For each node \eqn{i, i = 1,...,n} we aim to test the hypotheses \eqn{H_0}:
#' node \eqn{i} is not influential to the network, versus \eqn{H_1}: node \eqn{i}
#' is influential to the network.
#'
#' Define \eqn{q\in [0, 1]} to be the upper quantile cut off value and \eqn{B} to
#' be the number of bootstrap samples.
#'
#' The JaB algorithm has three steps:
#'
#' 1. **Original Centrality Step**: Calculate \eqn{n} centrality statistics of the
#' original network, \eqn{\gamma_1, \gamma_2, ..., \gamma_n}.
#'
#' 2. **Bootstrapping Step**: Generate \eqn{B} bootstrap samples.
#'
#' 3. **Jackknife-after Step**: For \eqn{i \in 1,...,n}, do:
#'
#'    A. Find all bootstrap samples that *do not* contain node \eqn{i} and store all of their respective centrality statistics in one vector, \eqn{V_i}.
#'
#'    B. Calculate \eqn{q_i}, the \eqn{q^{\text{th}}} quantile of \eqn{V_i}.
#'
#'    C. If \eqn{\gamma_i > q_i} then we reject the null hypothesis for node \eqn{i}
#'    and conclude that node \eqn{i} is influential.
#'
#' Once we run the algorithm, we have a set of nodes that are influential a set of nodes that are not.
#'
#' We can also generate a ranking of all nodes from most to least influential
#' determined by \eqn{\gamma_i - q_i} in step 3C. Node \eqn{i} is considered influential
#' when \eqn{\gamma_i - q_i} is large and positive, somewhat influential when \eqn{\gamma_i - q_i}
#' is small and positive, somewhat not influential when \eqn{\gamma_i - q_i}, and
#' very not influential when \eqn{\gamma_i - q_i} is large and negative.
#'
#' `jab_network` is a wrapper function for the entire JaB algorithm. For each step of the JaB algorithm,
#' the following functions are used:
#'
#' 1. **Original Centrality Step**: Calculate the original centrality statistics using [get_centrality()] according to `central.func.name`.
#' 2.  **Bootstrapping Step**: Generate Bootstrap samples according to `bootstrap.func.name` and calculate their
#'   centrality statistics using [get_bootstrap_centrality()].
#' 3. **Jackknife-after Step**: Perform the "Jackknife-after" step of the algorithm with [get_jackknife_after()].
#'
#' See `vignette("jab-networks")` for more details and examples.
#'
#' @seealso [get_centrality()],  [get_bootstrap_centrality()], [get_jackknife_after()]
#'
#'
#' @examples
#' library(igraphdata)
#' library(igraph)
#' data(karate)
#'
#' # JaB with snowboot (1 seed and 2 waves), 1000 bootstrap samples,
#' # degree (from igraph), and cutoff quantile of 0.90
#'
#' jab_network(
#'   network = karate,
#'   central.func.name = "degree",
#'   central.package.name = "igraph",
#'   central.func.args = list(normalized = TRUE),
#'   bootstrap.func.name = "bootstrap_snowboot" ,
#'   bootstrap.package.name = "JaB" ,
#'   bootstrap.func.args = list( num.seed = 1, num.wave = 2 ),
#'   B = 1000,
#'   quant = 0.90,
#'   nodes = NULL,
#'   return.boot.samples = FALSE)
#' @export
jab_network <- function(network,
                        central.func.name,
                        central.package.name = NULL,
                        central.func.args = NULL,
                        bootstrap.func.name,
                        bootstrap.package.name = NULL ,
                        bootstrap.func.args = NULL,
                        B = 1000,
                        quant = 0.95,
                        nodes = NULL,
                        return.boot.samples = FALSE){


  #ensure network is igraph object
  class <- JaB::detect_type(list(network))
  if(class != "igraph"){
    stop("network argument must be an igraph object.")
  }

  # Get Bootstrap Samples
  if(!is.null(bootstrap.func.args)){
    bootstrap.func.args[["network"]] <- network
  }else{
    bootstrap.func.args <- list(network = network)
  }
  bootstrap.func.args[["B"]] <- B

  if(!is.null(bootstrap.package.name)){
    bootstrap.func.name = getExportedValue(bootstrap.package.name, bootstrap.func.name)
  }

  boot.result <- do.call(
    what = bootstrap.func.name,
    args = bootstrap.func.args
  )


  # Get Jackknife After
  jack.after <-
    get_jackknife_after(network,
                        boot.result,
                        quant,
                        central.func.name,
                        central.package.name,
                        central.func.args,
                        nodes)

  # Return
  if(!return.boot.samples){
    return(jack.after)
  }else{
    return(
      list(bootstrap = boot.result,
           jack.after = jack.after))
  }

}
