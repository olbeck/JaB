#### Script to Wrangle data

### Paul Revere Data Set -------------------------
### by David Hackett Fisher

#https://github.com/kjhealy/revere
library(igraph)
pr.data <- as.matrix(read.csv("data-raw/paul-revere-raw-data.csv",row.names=1))
people = rownames(pr.data)

person.net <- pr.data %*% t(pr.data)
group.net <- t(pr.data) %*% pr.data


diag(group.net) <- 0
diag(person.net) <- 0

#checks
sum(person.net > 0) / 2  #9706
sum(group.net > 0 ) /2  #19

dim(pr.data)

pr.data <- as.data.frame(pr.data)



paul.revere <-
  igraph::graph_from_adjacency_matrix(
    person.net,mode="undirected",
    weighted=TRUE, diag=FALSE)

## Adding Individual Membership attributes
vertex.attributes(paul.revere)$StAndrewsLodge <- pr.data$StAndrewsLodge == 1
vertex.attributes(paul.revere)$LoyalNine <- pr.data$LoyalNine == 1
vertex.attributes(paul.revere)$NorthCaucus <- pr.data$NorthCaucus == 1
vertex.attributes(paul.revere)$LongRoomClub <- pr.data$LongRoomClub == 1
vertex.attributes(paul.revere)$TeaParty <- pr.data$TeaParty == 1
vertex.attributes(paul.revere)$BostonCommittee <- pr.data$BostonCommittee == 1
vertex.attributes(paul.revere)$LondonEnemies <- pr.data$LondonEnemies == 1


## Group Membership
group_membership <- vector(mode = "list", length = dim(group.net)[1])
names(group_membership) <- colnames(pr.data)
for(i in 1:length(group_membership)){
  group_membership[[i]] <- people[which(pr.data[, i] == 1)]
}

edge.list <- as_edgelist(paul.revere)
edge.list.membership <- vector(mode = "list", length = nrow(edge.list))

for(i in 1:nrow(edge.list)){
  v1 <- edge.list[i, 1]
  v2 <- edge.list[i, 2]

  in1 <- unlist(lapply(group_membership, function(x){v1 %in% x}))
  in2 <- unlist(lapply(group_membership, function(x){v2 %in% x}))

  edge.list.membership[[i]] <- names(group_membership)[in1 & in2]
}

#is #of shared orgs = edge weight
all(unlist(lapply(edge.list.membership, length)) == E(paul.revere)$weight)

paul.revere <- set_edge_attr(paul.revere, 'membership', value=edge.list.membership)









usethis::use_data(paul.revere, overwrite = TRUE)

paul.revere.groups <-
  igraph::graph_from_adjacency_matrix(
    group.net, weighted=TRUE,
    mode="undirected", diag=FALSE)

usethis::use_data(paul.revere.groups, overwrite = TRUE)

### Karate Data set ---------------------------------
# data loaded from igraphdata package
data(karate, package = "igraphdata")
usethis::use_data(karate)

