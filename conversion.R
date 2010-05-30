 source(jPaste(SBM_PATH, "likelihood.R"))

library(graph)

extractNodes <- function(){
  data <- read.delim(file="edgeRankingData.txt")
  col1 <- jPaste(data$orf1)
  col2 <- jPaste(data$orf2)
  sort(unique(c(col1,col2)))
}

initFromRealData <- function(k){
  jCat("entered initFromRealData")
  Nodes <<- extractNodes()
  (nv <<- length(Nodes))                     # number of nodes
  (ne <<- choose(nv, 2))                     # number of edges
  data <- read.delim(file="networkData.txt")
  numLabels <<- k
  availableLetters <<- setdiff(LETTERS,LETTERS[1:k])
}


  ##eList <<- lapply(seq_along(vVect), function(i) { return(list(edges = (seq_along(vVect)[-i])))}) ## ToDo: use 1:n, (1:n)[-i]
  ##names(eList) <<- vVect
  ##(kn <<- new("graphNEL", nodes = vVect, edgeL = eList))


convertEdgeRankingDataFile <- function(){
 data <- read.delim(file="edgeRankingData.txt")
 col1 <- jPaste(data$orf1)
 col2 <- jPaste(data$orf2)
 ranking <- c()
 for (i in 1:length(col1)){
   ranking[i] <- jPaste(col1[i],"~",col2[i])   
 }
 save(ranking,file="ranking")
 ranking
}



convertNetworkDataFile <- function(){
 data <- read.delim(file="networkData.txt")
 network <- new("graphNEL", nodes=Nodes)
 col1 <- jPaste(data$ProteinA)
 col2 <- jPaste(data$ProteinB)
 for (i in 1:length(col1))
   network <- addEdge(col1[i],col2[i],network,1)
 
 for (edge in edgeNames(complement(network))){
   node1 <- parseTilde(edge)[1]
   node2 <- parseTilde(edge)[2]
   network <- addEdge(node1,node2,network,0)
 } 
 save(network, file="network")
}



