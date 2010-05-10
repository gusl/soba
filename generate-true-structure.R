source(jPaste(SBM_PATH, "visualize.R"))
source(jPaste(SBM_PATH, "mcmc.R"))
library(igraph)

(trueLabeling <- jPaste(config$truth))
sMod <- computeSModVec(cz(trueLabeling))

init(sMod)

##make a vList 


##ToDo: stop using binary; read strings instead

initTest <- function(){
  Nodes <- unlist(vList, use.names=FALSE)
  structure <- Reduce(function(a,b) jPaste(a,b), sapply(Nodes,getLetter)); write(file="truth.tex",trueLabeling)
  numLabels <- length(vList); write(file="numLabels.tex",numLabels); Labels <- LETTERS[1:numLabels]
  ##rtrue <<- 10;
  write(file="r.tex",config$rtrue)
  list(Nodes=Nodes,sMod=sMod,structure=structure,numLabels=numLabels,Labels=Labels,r=config$rtrue, gamma=config$gamma, delta=config$delta)
}

truth <- initTest()

##from now on we will use: truth$r, truth$structure (instead of 'truth'), truth$Nodes, truth$Labels, etc


clusterList <- list(c(1:10), c(11:13), c(14:20))
gr2 <- new("clusterGraph", clusters=vList)

write(file="truestructure.tex", formatVector(sMod))

##if (doPlots){
##   pdf("slide-truestructure.pdf")
##   visualizeStructure(gr2,title="")
##   dev.off();
## }
save(truth,file="truth")
