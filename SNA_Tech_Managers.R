library(igraph)
library(ggplot2)
library(tidyverse)
library(ggthemes)
library(knitr)
library(dplyr)

# analyzing friendship relationships between 21 managers at a tech company
# loading data from http://moreno.ss.uci.edu/data.html
man_tech <- as.matrix(read.table("http://moreno.ss.uci.edu/krackht.dat", skip = 8))
friend_tech <- man_tech[22:42, ] 
att_tech <-  as.data.frame(read.table("http://moreno.ss.uci.edu/krackht_att.dat",
                                      skip = 9), header = F)
colnames(friend_tech) <- 1:dim(friend_tech)[2]
att_tech_cnames <- c("Age",
                     "Tenure",
                     "Level",
                     "Department")
colnames(att_tech) <- att_tech_cnames
att_tech$ID <- 1:dim(att_tech)[1]

# distribution of attributes
att_tech[ , 1:4] %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_bar()

# SNA measures
phys_network_graph <- graph.adjacency(friend_tech, mode = "directed",
                                      weighted = NULL, diag = F) 

vertex_attr(phys_network_graph, index = att_tech$ID) <- att_tech

graph.density(phys_network_graph) 


phys_att <- merge(att_tech, data.frame(  
  ID = V(phys_network_graph)$ID,
  degree = degree(phys_network_graph, normalized = F),
  in_deg = degree(phys_network_graph, mode = c("in"),
                  loops = TRUE, normalized = F),
  out_deg = degree(phys_network_graph, mode = c("out"),
                   loops = TRUE, normalized = F),
  btwn = betweenness(phys_network_graph, directed = T,
                     normalized = T),
  close = closeness(phys_network_graph, mode = c("all"),
                    normalized = T)),
  by='ID')

# graphing SNA relationships
V(phys_network_graph)$color <- vector(length = dim(att_tech)[1])
V(phys_network_graph)[V(phys_network_graph)$Level == 1]$color <- "red"
V(phys_network_graph)[V(phys_network_graph)$Level == 2]$color <- "lightgreen"
V(phys_network_graph)[V(phys_network_graph)$Level == 3]$color <- "lightblue"


V(phys_network_graph)$size <- 100 * betweenness(phys_network_graph, normalized = T)

# color by level
# size by betweenness centrality
l <- layout.kamada.kawai(phys_network_graph)
plot(phys_network_graph, layout = l, edge.arrow.size=.1)

# finding groups with top-down methods
phys_network_graph2 <- graph.adjacency(friend_tech, mode="min", weighted=NULL) 
graph.density(phys_network_graph2) 
# Girvan Newman algorithm
friend_gn <- edge.betweenness.community(phys_network_graph2,
                                        directed = TRUE, edge.betweenness = TRUE, 
                                        merges = TRUE, bridges = TRUE, 
                                        modularity = TRUE, membership = TRUE) 
# random walk algorithm
friend_rw <- walktrap.community(phys_network_graph2, steps=200, modularity=TRUE) 

# group memberships
friend_gn_memb <- data.frame(friend_gn$membership)
friend_rw_memb <- data.frame(friend_rw$membership)
# seven groups from Girvan Newman
table(friend_gn_memb)
# six groups from random walk
table(friend_rw_memb)
group_compare <- cbind(friend_gn_memb, friend_rw_memb)
colnames(group_compare) <- c("girvan_newman", "random_walk")
group_compare <- cbind(group_compare, att_tech)
group_compare <- group_compare %>%
  select(ID, everything())

## plot Girvan-Newman 
plot(friend_gn, phys_network_graph2) 
## plot random walk 
plot(friend_rw, phys_network_graph2)






