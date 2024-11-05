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
  network = paul.igraph,
  func.name ="closeness",
  package.name = "igraph",
  func.args = list(normalized = T))
test

test <-  get_centrality(
    func.name ="eigen_centrality",
    network = paul.igraph,
    package.name = "igraph",
    func.args = NULL)
test




## Testing Latent space bootstrap ---------------------
Z <- ASE(paul.igraph, 2)

test


test<-
  bootstrap_latent_space(paul.igraph, d=2, B=10,
                         output.type = "matrix")
test[[1]]
input.type <- detect_type(test)
make_network_type(test, input.type, "igraph")


get_bootstrap_centrality(test,
                         "degree",
                         package.name = NULL,
                         func.args = NULL)






### Karate example for snowboot -------------------------
library(igraphdata)
data(karate)
karate
network <- karate
B=5
num.seed = 2
num.wave = 2
seeds = NULL

test <- bootstrap_snowboot(
  karate, B = 1000,
  num.seed = 1, num.wave = 2,
  output.type = "matrix")
lapply(test,dim)

boot_result <- test

test[[1]]
JaB:::detect_type(boot_result)

make_network_type(test, JaB:::detect_type(test), "igraph")


test <- bootstrap_vertex(karate, output.type = "edgelist")
test[[1]]

###
boot_result <- bootstrap_snowboot(
  karate, B = 100,
  num.seed = 1, num.wave = 1,
  output.type = "matrix")

network <- karate
boot.result <- test
quants <- c(0, 0.9)
func.name = "degree"
package.name = NULL
func.args = NULL

central_result <- get_bootstrap_centrality(boot_result,
                                           func.name = "degree",
                                           package.name = "igraph",
                                           func.args = list(normalized = TRUE))

get_jackknife_after(karate, test, c(0, 0.9), "closeness")


data(karate)
boot.result <- bootstrap_snowboot(
  karate, B = 100,
  num.seed = 2, num.wave = 2,
  output.type = "matrix")


View(temp)


data("paul.revere")

boot.result <- bootstrap_snowboot(
  paul.revere, B = 1000,
  num.seed = 1, num.wave = 2,
  output.type = "matrix")
temp <-get_jackknife_after(paul.revere, boot.result, 0.95,
                           "betweenness",
                           func.args = list(normalized = TRUE))
temp %>% View
