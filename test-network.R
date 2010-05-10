source("~/.Rprofile")
source("likelihood.R")
init(c(3,5,6))
g <- generateNetwork(0.9,0.1)

library(Rgraphviz)
source("visualize.R")
visualizeStructure(g)
