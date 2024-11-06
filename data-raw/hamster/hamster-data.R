# # download
# pth <- "http://nrvis.com/download/data/misc/actor-movie.zip"
# download.file(pth, destfile = "actor-movie.zip")
#
# # see file names
# unzip("actor-movie.zip", list = TRUE)
#
# # unzip

#
unz <- ("data-raw/hamster/soc-hamsterster.edges")
#
#
# #https://networkrepository.com/soc-hamsterster.php
# # quick look : looks like edge list
# readLines(unz, n=10)

# skip first line to avoid % bipartite unweighted"
dat <- read.table(unz, skip=2, sep=",")


#format for edgelist
library(tidyverse)
dat <-
  dat %>%
  rename(both = V1) %>%
  mutate(V1 = word(both, 1, 1),
         V2 = word(both, 2, 2))



# look
head(dat)
str(dat)

# load as a graph
library(igraph)

g <- graph_from_data_frame(dat[, c("V1", "V2")], directed = F)
g
plot(g,
     vertex.label = NA,
     vertex.size = 3)

hamster <- g

usethis::use_data(hamster)

