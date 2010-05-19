source("~/.Rprofile")
source("likelihood.R")
init(c(3,5,6))


gamma <- 0.5
delta <- 0.1

g <- generateNetwork(gamma,delta,1,1) ## NO EDGES MISSING
getEdgeWeights(g)


g <- generateNetwork(gamma,delta,1,0.1) ## ABSENT: not measured => absent i.e. present => measured
getEdgeWeights(g)


g <- generateNetwork(gamma,delta,0.1,0.1) ## MAR
getEdgeWeights(g)


g <- generateNetwork(gamma,delta,0.5,0.1) ## MORE REALISTIC
getEdgeWeights(g)




library(Rgraphviz)
source("visualize.R")
visualizeStructure(g)


source("mcmc.R")
