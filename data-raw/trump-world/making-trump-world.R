### Making Trump World Data
library(tidyverse)
library(igraph)


####################
### Import data
####################
dat.trump <- read_csv("data-raw/trump-world/TrumpWorldData.csv")

####################
### Making it an igraph
######################

trump_igraph <- graph_from_edgelist(as.matrix(dat.trump[, c(2,4)]), directed = F )
n <- gorder(trump_igraph)

# Get node names and IDs
orig_nodes <- V(trump_igraph)$name
V(trump_igraph)$ID <- 1:n

# Get other node attributes -------------------
entity.info <-
  dat.trump[, 1:2] %>%
  rename(EntityType = `Entity A Type`,
         Entity = `Entity A`) %>%
  rbind(
    dat.trump[, 3:4]%>%
      rename(EntityType = `Entity B Type`,
             Entity = `Entity B`)
  ) %>%
  distinct() %>%
  arrange(match( Entity, V(trump_igraph)$name))

# make sure the number of nodes and entities are the same
n
nrow(entity.info)

#make sure the names match
all(entity.info$Entity == V(trump_igraph)$name)

#add entity type to igraph
V(trump_igraph)$EntityType <- entity.info$EntityType


### Add Edge attributes --------------------------
E(trump_igraph)$Connection <- dat.trump$Connection
E(trump_igraph)$Source <- dat.trump$`Source(s)`


### Save data set ----------------------
trump.world <- trump_igraph
usethis::use_data(trump.world)
