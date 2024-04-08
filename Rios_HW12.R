# Installing Libraries 
library(GGally)
library(network)
library(sna)
library(ggplot2)
library(RColorBrewer)
#------------------

# Ex. 1: Random Graph 
net = rgraph(10, mode = "graph", tprob = 0.5)
net = network(net, directed = FALSE)

## vertex names
network.vertex.names(net) = letters[1:10]

# Creating Graphic 
ggnet2(net, size = 6, node.color = rep(c("orange", "tomato"), 5))

#------------------

# Ex. 4: French MPs

## Reading data- notes and edges
r = "https://raw.githubusercontent.com/briatte/ggnet/master/"

### read nodes
v = read.csv(paste0(r, "inst/extdata/nodes.tsv"), sep = "\t")
names(v)

### read edges
e = read.csv(paste0(r, "inst/extdata/network.tsv"), sep = "\t")
names(e)

## network object
net = network(e, directed = TRUE)

## party affiliation
x = data.frame(Twitter = network.vertex.names(net))
x = merge(x, v, by = "Twitter", sort = FALSE)$Groupe
x = factor(x)
net %v% "party" = as.character(x) 

##color palette
y = RColorBrewer::brewer.pal(9, "Set1")[ c(3, 1, 9, 6, 8, 5, 2) ]
names(y) = levels(x)

## network plot
ggnet2(net, color = "party", palette = y, alpha = 0.75, size = 4, edge.alpha = 0.5)

