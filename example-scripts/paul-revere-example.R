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


### Testing get_centrality --------------------
test <- get_centrality(
  func.name ="closeness",
  graph = paul.igraph,
  package.name = "igraph",
  func.args = list(normalized = T))
test

test <-  get_centrality(
    func.name ="eigen_centrality",
    graph = paul.igraph,
    package.name = "igraph",
    func.args = NULL)
test


## Testing Latent space bootstrap ---------------------
Z <- ASE(paul.igraph, 2)
test<-
  bootstrap_latent_space(paul.igraph, d=2, B=10,
                         output.type = "sparsematrix")
test

get_bootstrap_centrality(test,
                         "sparsematrix",
                         "degree",
                         package.name = NULL,
                         func.args = NULL)



### Karate example for snowboot -------------------------
library(igraphdata)
data(karate)
karate
graph <- karate
num.seed = 2
num.wave = 2
seeds = NULL

bootstrap_snowboot(
  karate, B = 10,
  num.seed = 1, num.wave = 3,
  seeds = NULL,
  output.type = "edgelist")


