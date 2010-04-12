##OBSOLETE - USE FOR REFERENCE ONLY
## Use make-slides.sh instead




## Best run this way:  C:/Progra~1/R/R-2.9.1/bin/Rscript.exe make-slides.R
##setwd("C:/projects/sbm/R")
source("~/.Rprofile")
source("visualize.R")
source("mcmc.R")
##setwd("..")
library(igraph)

(dirName <- jPaste(floor(10^8*runif(1)))); system(jPaste("mkdir ",dirName)); setwd(dirName); system("cp ../slides.tex .")

initTest <- function(){
  ##Nodes <<- unlist(vList, use.names=FALSE)
  truth <<- Reduce(function(a,b) jPaste(a,b), sapply(Nodes,getLetter)); write(file="truth.tex",truth)
  numLabels <<- length(vList); write(file="numLabels.tex",numLabels); Labels <<- LETTERS[1:numLabels]
  rtrue <<- 10; write(file="r.tex",rtrue)
  ##(ranking <<- doExpt(rtrue,1)$arrivals[[1]]) ##(labeling <<- convertVListToVec(vList))  ##objective <<- function(model) loglikVec(ranking,model,rtrue) ##prop <<- function(x) gProposal(x,2)
}


init(c(3,5,6))
##init(c(3,2,1))

initTest()

## SLIDE - true structure

## SLIDE - observed edge ranking
#### 1D heatmap of edge ranking, showing "within" as orange, "between" as blue
#### 2D heatmap of edge ranking, showing earlier as red

## SLIDE - inferred structures
#### MAP
#### heatmap of Rand index

## SLIDE - colabeling probabilities
####

## SLIDE - posterior mean (via graph clustering)


#######################################################################################



########## SLIDE - true structure ##########
##eList <- lapply(seq_along(vVect), function(i) { return(list(edges = (seq_along(vVect)[-i])))})


##eList <- list( list(edges = c(2)), list(edges = c(1)))
##gr <- new("graphNEL", nodes = c("A", "B"))##, edgeL = eList, edgemode="directed")

clusterList <- list(c(1:10), c(11:13), c(14:20))
gr2 <- new("clusterGraph", clusters=vList)##clusterList)
##gr3 <- new("clusterGraph", clusters=vList2)

write(file="truestructure.tex", formatVector(sMod))

pdf("slide-truestructure.pdf")
visualizeStructure(gr2,title="")
##visualizeStructure(gr3,title="")
dev.off();
##dev.print(pdf,"slide-truestructure.pdf")


########## SLIDE - observed edge ranking ##########
(run <- doExpt(rtrue,1))

pdf("slide-ranking.pdf",height=2.5,width=7)
plotRuns(run,title="",makeText=FALSE,makeBalls=TRUE)
dev.off()

(ranking <- run$arrivals[[1]])
pdf("slide-ranking2.pdf")
makeHeatmapRanking(ranking)
dev.off()


########## SLIDE - stochastic search ##########

nIter <- 2000; nIterPerRestart <- 1000; jumpSize <- 2
write(file="nIter.tex", nIter)
write(file="nIterPerRestart.tex", nIterPerRestart)
write(file="jumpSize.tex", jumpSize)

(Labels <- LETTERS[1:numLabels]) ##is this needed?
(initial <- randomLabeling(length(Nodes)))
objective <- function(model) loglikVec(ranking,model,rtrue)
prop <- function(x) gProposalFixedNLabels(x,jumpSize,numLabels)
##prop <- function(x) gProposal(x,jumpSize)

ssRun <- sSearch(prop, initial, objective, nIter, restart=nIterPerRestart, logNeglect=1000)

save(file="ssRun", ssRun)

load(file="ssRun")

samplesHash <- ssRun$samples
mass <- ssRun$mass
nModels <- ssRun$nModels ##number of models visited
pdf("slide-stochastic_search.pdf")
plotStochasticSearchProgress(ssRun)
dev.off()

sortedModels <- sort(values(ssRun$samples), decreasing=TRUE)
pdf("slide-posterior_mass.pdf")
plot(exp(sortedModels), type="h", xlab="rank", ylab="mass")
dev.off()
truthIndex <- match(truth, names(sortedModels))

if (!is.na(truthIndex)){ ##truth found
  write(file="truthIndex.tex", jPaste("The truth was the ",truthIndex,"th highest-scoring model found."))
} else {
  postT <- sapply(sortedModels, function(x) exp(x))
  totalMass <- sum(postT)+exp(objective(cz(truth)))
  proportionT <- exp(objective(cz(truth)))/totalMass
  write(file="truthIndex.tex", jPaste("The truth was not found. If we add it to this set, it has ", signif(100*proportionT,3), "\\% of the mass."))
}


########## SLIDE - inferred structures ##########
models <- keys(samplesHash) ##to be modified shortly (see 4 lines down)
post <- sapply(models, function(x) exp(objective(cz(x)))) ##ToDo: post <- values(samplesHash)
totalMass <- sum(post) ##(totalMass <- postMass(ssRun$samples,objective))
proportion <- sort(post, decreasing = TRUE)/totalMass
models <- names(proportion)

pdf("slide-posterior.pdf", height=5)
makeHeatmapPostRI(post,randIndex,12,makeText=TRUE, truth=truth)
dev.off()

nBlocks <- sapply(models,countBlocks)
##hist(nBlocks)
entropies <- sapply(models,modelEntropy)
risFromTruth <- sapply(models, function(s) randIndex(cz(s),cz(truth)))

##ToDo: weighted average! Weight sd!

## now do the model averaging
write(file="entropies.tex", jPaste("mean = ", signif(weighted.mean(entropies,proportion),3)))
write(file="nBlocks.tex", jPaste("mean = ", signif(weighted.mean(nBlocks,proportion),3)))
write(file="risFromTruth.tex", jPaste("mean = ", signif(weighted.mean(risFromTruth,proportion),3)))
## if distribution is bimodal, we would see some block structure (under some permutation)

########## SLIDE - colabeling probabilities ##########
colabelingProbs <- computeColabelingProbs(samplesHash)

pdf("slide-colabelingprobs.pdf")
makeHeatmapColabeling(colabelingProbs)
dev.off()

########## COMPILE all slides into a single PDF ##########
system("C:/\"Program Files\"/\"MiKTeX 2.7\"/miktex/bin/pdflatex slides.tex")




##system(jPaste("cp slides.pdf slides",dirName,".pdf"))


mat <- t(matrix(c(c(0,0,0,0,0), ##lower-triangular
                   c(1,0,0,0,0),
                   c(1,5,0,0,0),
                   c(5,0,0,0,0),
                   c(0,1,2,3,0))
                 ,nrow=5))

##example from http://www.cs.bgu.ac.il/~visproj/eransagi/flow.html

matIL <- t(matrix(c(c(0,0,0,0,0,0), ##lower-triangular
                    c(1,0,0,0,0,0),
                    c(7,1,0,0,0,0),
                    c(0,3,0,0,0,0),
                    c(0,2,4,1,0,0),
                    c(0,0,0,6,2,0))
                  ,nrow=6))


makeEdgeList <- function(mat){
  n <- nrow(mat)
  l <- c()
  for (j in 1:(n-1)){
    jCat("n = ", n)
    for (i in (j+1):n){ ##row
      jCat("n = ", n)
      jCat("i=",i,", j=",j,": ", mat[i,j])
      l <- c(l,mat[i,j])
    }
  }
  l
}

##makeEdgeList(mat)

## mat : the edgeweight matrix
## calls igraph's graph.mincut, which does not use source/target
mincut <- function(mat, nodeNames){
  n <- nrow(mat)  
  ##  if (n==2)
  ##    return
  ##  else{
  g <- graph.full(n,directed=FALSE)
  (cut <- graph.mincut(g,value.only=FALSE, capacity=makeEdgeList(mat)))

  part1Ind <- cut$partition1 + 1 ##to index from 1 rather than 0
  part2Ind <- setdiff(1:n,part1Ind)
  n1 <- nodeNames[part1Ind]
  n2 <- nodeNames[part2Ind]

  part1ol <- rep(0,length(n1)); part2ol <- rep(0,length(n2))
  ##"outside links": count each node's responsibility for the cut value
  for (i1 in seq_along(part1Ind)){
    ind1 <- part1Ind[i1] ##part1Ind refers to the indices in Part 1
    for (i2 in seq_along(part2Ind)){
      ind2 <- part2Ind[i2]
      ##jCat("i1=",i1,", i2=",i2, "  ind1=", ind1,", ind2=", ind2,"  mat[ind1,ind2]=", mat[ind1,ind2], "  mat[ind2,ind1]=", mat[ind2,ind1])
      part1ol[i1] <- part1ol[i1] + mat[ind1,ind2] + mat[ind2,ind1]
      part2ol[i2] <- part2ol[i2] + mat[ind1,ind2] + mat[ind2,ind1]
    }
  }
  list(value=cut$value, i1=part1Ind, i2=part2Ind, n1=n1, n2=n2,
       o1=part1ol, o2=part2ol)
}

##mc <- mincut(mat,LETTERS)

##return node-names, rather than indices

## ToDo:
## 
## * merge all the 'partit' into a single list
##


##implement mincut for graphNEL
##we will need to take the subgraph, and the corresponding weight matrix

## returns a min-cut tree
## works by expanding a single node into a tree
gomoryHu <- function(mat,nodeNames){
  n <- nrow(mat)

  initialPartition <- list(indices=1:n, names=nodeNames[1:n], ol=rep(0,n), branchName="", branchWeight=NA)
  partit <- list(initialPartition)
  
  nPartit <- length(partit)
  while(nPartit<n){

    treeNodeIndices <- sample(1:(length(partit))); count <- 0 ##permute the indices
    while(TRUE){ ##pick an element of partit containing more than one node
      count <- count+1
      treeNodeIndex <-  treeNodeIndices[count]
      currentTreeNode <- partit[[treeNodeIndex]]
      nTreeNode <- sum(!is.na(currentTreeNode$names)) ##do not count NA as nodes
      if (nTreeNode>1) break
    }
    
    ##add one more row to submat, corresponding to OUTSIDE node
    ##where each edge is the sum of the outside links
    submat <- mat[currentTreeNode$indices,currentTreeNode$indices]
    if (sum(currentTreeNode$ol)>0) ##if there is an outside...
      submat <- cbind(rbind(submat,currentTreeNode$ol),rep(0,nTreeNode+1)) ## add row
     
    mc <- mincut(submat, currentTreeNode$names) ##c(, "OUTSIDE")
    partit <- partit[-treeNodeIndex] ##delete currentTreeNode from 'partit'
    
    nPartit <- length(partit) ##and add its two subsets
    
    ##the "connecting branch", i.e. the side that gets the NA, gets the "0" appended
    connectingBranchName <- jPaste(currentTreeNode$branchName,"0")
    isolatedBranchName <- jPaste(currentTreeNode$branchName,"1")

    ##exactly 1 NA ("OUTSIDE") will show up, in either mc$n1 or mc$n2

    if(sum(is.na(mc$n1))==1) { ##need to fix the condition
      connectingBranch <- list(indices=mc$i1, names=mc$n1, ol=mc$o1+mc$value, branchName=connectingBranchName, branchWeight=currentTreeNode$branchWeight) ##inherits value of old cut
      isolatedBranch <- list(indices=mc$i2, names=mc$n2, ol=mc$o2+mc$value, branchName=isolatedBranchName, branchWeight=mc$value) ##value of the new cut
    } else {
      connectingBranch <- list(indices=mc$i2, names=mc$n2, ol=mc$o2+mc$value, branchName=connectingBranchName, branchWeight=currentTreeNode$branchWeight) ##inherits value of old cut
      isolatedBranch <- list(indices=mc$i1, names=mc$n1, ol=mc$o1+mc$value, branchName=isolatedBranchName, branchWeight=mc$value) ##value of the new cut   
    }
    
    partit[[nPartit+1]] <- connectingBranch
    partit[[nPartit+2]] <- isolatedBranch
  }
}
## give names to the elements of 'partit'
## "0" connects to the outside

##double NA should be impossible!

##debug(gomoryHu)
##gomoryHu(mat,LETTERS[1:n])

##gomoryHu(matIL,jPaste(0:5))


##partition2 sometimes gives an alternative partition  

## The NA (meaning OUTSIDE) should not get added to the
##
## The min-cut tree should be constructed based on this.
##
##
##
