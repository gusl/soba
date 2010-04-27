source(jPaste(SBM_PATH, "mcmc.R"))
load(file="truth")
load(file="ssRun")
load(file="colabelingProbs")
config <- read.csv("config.csv")


jCat("truth$structure = ", truth$structure)

##ToDo: instead of matrix, do it for the entries in the lower-triangle.

matrixDistance <- function(mat1, mat2) {
  ##print("mat1 = ", mat1); print("mat2 = ", mat2)
  sum((mat1 - mat2)^2)
}

colabPrCost <- function(labeling, colabPr) {
  strMatrix <- makeStructureMatrix(labeling)
##  jCat("strMatrix:   rows: ", nrow(strMatrix), ", cols: ", ncol(strMatrix))
##  jCat("colabPr:   rows: ", nrow(colabPr), ", cols: ", ncol(colabPr))
  matrixDistance(colabPr, strMatrix)
}

makeStructureMatrix <- function(labeling){
  lab <- cz(labeling)
  n <- length(lab)
  mat <- matrix(rep(0,n^2), nrow=n)
  for (i in 1:n)
    for (j in 1:n)
      if (lab[i]==lab[j])
        mat[i,j] <- 1
  mat
}


cutCost <- function(labeling, colPrMat) {
  strMatrix <- makeStructureMatrix(labeling)
  (1-strMatrix) ##this is the "between" edges
  sum((1-strMatrix)*colPrMat) + 100000*(truth$numLabels-labelCount(labeling))
}

labelCount <- function(labeling) length(unique(cz(labeling)))


##labeling is a string
##returns a list of strings, namely structures that are one relabeling away from 'labeling'.
singleNbhd <- function(labeling){
  lab <- cz(labeling)
  
  set <- list()
  set[1] <- labeling ##relation is reflexive
  count <- 1
  for(i in 1:length(lab)){ ## for each position
    currentLabel <- match(lab[i],LETTERS)-1
    newLabeling <- lab
    
    for (j in 1:(truth$numLabels-1)){ ## for each relabeling
      count <- count+1
      newLabel <- (currentLabel+j)%%(truth$numLabels) ##mod k      
      newLabeling[i] <- LETTERS[newLabel+1]
      set[count] <- Reduce(jPaste,newLabeling)
    }
  }
  set
}
##debug(singleNbhd)
##singleNbhd("AA") ##singleNbhd("AABBBC")

##'currentL' is a string, namely the current labeling
greedySearch <- function(objective, nbhd, initial){
  ##start from a random labeling
  currentLab <- initial
  prevCurrentLab <- "null"
  count <- 0
  while(count<2*length(truth$Nodes)) ## && currentLab!=prevCurrentLab)
    ##hypothesis: it always stops after
    {
      count <- count+1
      neighbors <- nbhd(currentLab)
      objs <- sapply(neighbors, objective)
      bestIndex <- which.min(objs)
      prevCurrentLab <- currentLab
      currentLab <- neighbors[[bestIndex]]
    }
  currentLab
}


## colabPrCost objective
obj <- function(labeling) colabPrCost(labeling,colPrMat)

initial <- Reduce(jPaste,randomLabeling(nrow(colPrMat)))
consensusColpr <- greedySearch(obj, singleNbhd, initial)
normalizedConsensusColpr <- concat(normalForm(cz(consensusColpr)))
write(normalizedConsensusColpr, file="consensus-colabeling.tex")
##h <- test()


## cutCost objective
obj <- function(labeling) cutCost(labeling,colPrMat)
initial <- Reduce(jPaste,randomLabeling(nrow(colPrMat)))
(consensusCut <- greedySearch(obj, singleNbhd, initial))
(normalizedConsensusCut <- concat(normalForm(cz(consensusCut))))
write(normalizedConsensusCut, file="consensus-mincut-greedy.tex")
##h <- test()


## See if the greedy search always returns the same answer
test <- function(){
  h <- hash()
  for (i in 1:50){
    init <- Reduce(jPaste,randomLabeling(nrow(colPrMat)))
    consensus <- greedySearch(obj, singleNbhd, init)
    normalizedConsensus <- Reduce(jPaste,normalForm(cz(consensus)))
    jCat("normalizedConsensus = ", normalizedConsensus)
    if(!has.key(normalizedConsensus, h))
      h[[normalizedConsensus]] <- 1
    else
      h[[normalizedConsensus]] <- h[[normalizedConsensus]]+1
  }
  h
}


##jCat("config$truth = ", config$truth[[1]])
(riColpr <- randIndex(truth$structure, normalizedConsensusColpr))
(riCut <- randIndex(truth$structure, normalizedConsensusCut))

jCat("riColpr = ", riColpr)
jCat("riCut = ", riCut)
write(jPaste("colPrDist, ", normalizedConsensusColpr, ", ", riColpr), file="consensusResults.csv",  append=TRUE)
write(jPaste("cut, ", normalizedConsensusCut, ", ",  riCut), file="consensusResults.csv", append=TRUE)

## The first consensus is being computed in visualize.R


(thresholded <- (colPrMat > 0.5))

##assign different symbols to all nodes
(labs <- LETTERS[1:length(truth$Nodes)])
##if they are connected, they get the same letter as the left one
for (i in seq_along(truth$Nodes)){
  for (j in seq_along(truth$Nodes)){
    if (thresholded[i,j]){
      labs[j] <- labs[i]
    }
  }
}
consensusThresh <- concat(normalForm(labs))
riThresh <- randIndex(truth$structure, consensusThresh)
write(jPaste("thresh, ", consensusThresh, ", ", riThresh), file="consensusResults.csv", append=TRUE)

jCat("writing consensus")


## it's so easy, it's annoying

## posterior
models <- keys(ssRun$samples)
consensusMode <- models[which.max(post)]
riMode <- randIndex(truth$structure, consensusMode)
write(jPaste("mode, ", consensusMode, ", " , riMode), file="consensusResults.csv", append=TRUE)
