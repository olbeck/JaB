% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-bootstrap-centrality.R
\name{get_bootstrap_centrality}
\alias{get_bootstrap_centrality}
\title{Compute Bootstrap Centrality Statistics from Bootstrap Samples}
\usage{
get_bootstrap_centrality(
  boot.result,
  func.name,
  package.name = NULL,
  func.args = NULL,
  obj.type = NULL
)
}
\arguments{
\item{boot.result}{A list of bootstrap samples, where each sample represents a graph in a specified format (e.g., adjacency matrix, edgelist). This is output from the \code{bootstrap_} function.}

\item{func.name}{A character string specifying the centrality function to apply (e.g., \code{"degree"}, \code{"betweenness"}). See \code{\link{get_centrality}} for details.}

\item{package.name}{An optional character string indicating the package name containing the centrality function. Defaults to \code{NULL} (assuming \code{func.name} is available). See \code{\link{get_centrality}} for details.}

\item{func.args}{An optional list of additional arguments to pass to the centrality function. Defaults to \code{NULL}. See \code{\link{get_centrality}} for details.}

\item{obj.type}{A character string specifying the type of \code{obj} in each bootstrap sample. Options include \code{"igraph"}, \code{"matrix"}, \code{"dgCMatrix"}, or \code{"edgelist"}. This is the \code{output.type} of the \code{bootstrap_} function.}
}
\value{
A list of length \code{B}, where each element is the bootstrap centrality statistic result from applying the specified centrality statistic function.
}
\description{
This function takes bootstrap samples of graph data from \code{bootstrap_} functions and computes specified centrality statistics for each sample.
}
\details{
Calculates bootstrap centrality statistics from give bootstrap samples and a specified centrality statistic.
}
\examples{
library(igraph)
data("karate")
set.seed(89)
boot.result <- bootstrap_latent_space(
  karate, d = 2, B = 10)

#Calculate degree centrality (from igraph)
central.result <- get_bootstrap_centrality(boot.result,
                                           func.name = "degree",
                                           package.name = "igraph",
                                           func.args = list(normalized = TRUE))
central.result[[1]]

#plot bootstrap centralities vs original centrality
hist(unlist(central.result), freq = FALSE,
     xlab = "degree centrality", main = "Bootstrap Degree Centralities")
lines(density(igraph::degree(karate, normalized = TRUE)), col = "red")


}
\seealso{
\code{\link{make_network_type}}, \code{\link{get_centrality}}
}
