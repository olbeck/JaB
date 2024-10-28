### Simple example with Paul Revere data set
## used for test script

### Load data and make graph objects -------------------------
library(readr)
library(igraph)
pr_data <- read_csv("example-scripts/paul-revere-data.csv")
colnames(pr_data)[1] <- "Names"

# make adjacency matrix
pr_mat <- as.matrix(pr_data[ , -1])
pr_adjmat <- pr_mat %*% t(pr_mat)
diag(pr_adjmat) <- 0
colnames(pr_adjmat) <- rownames(pr_adjmat) <- pr_data$Names

pr_adjmat_unweighted <- ifelse(pr_adjmat>0, 1, 0)
diag(pr_adjmat_unweighted) <- 0


# make igraph object
paul.igraph <- graph_from_adjacency_matrix(
  pr_adjmat_unweighted,
  mode="undirected",
  weighted=NULL,
  diag=FALSE)


###

