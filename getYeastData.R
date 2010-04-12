## taken from http://www.stat.ubc.ca/~jenny/webSupp/schluterGlobDisc/schluterEtAl.R

##library(qvalue)

## read in pre-processed data
tmp <- read.csv("schluterEtAlData.csv", header = T, as.is=1:2)
##read.table
caData <- tmp[,-(1:2)]
rownames(caData) <- tmp$ORF

(caData <- caData[1:5,])

## store ORF and ORF pair counts
(G <- nrow(caData))                     # 279 ORFs
(GG <- G * (G - 1)/2)                   # 38781 ORF pairs

## compute euclidean distance and convert to ranks
(d1 <- dist(caData))
d2 <- d1
(d2[1:GG] <- rank(d1))

## what order is this in?
## does d2[1] give you the ranking of the A1 - A2 edge?


## Goal: write down the edge-names (i.e. node-pairs), so that we can use the code I've already written.

matrixMatch <- function (element,m){
  index <- match(element,m) - 1
  j <- floor(index/nrow(m))
  i <- index%%nrow(m)
  c(i+1,j+1)
}

##convert to Gustavo's format
convertToEdgeNameRanking <- function(distRanking){
  enRanking <- c()
  for (i in 1:length(distRanking)){
    indexVec <- matrixMatch(i,as.matrix(distRanking))
    ##jCat("i = ", i, ", indexVec = ", indexVec)
    enRanking[i] <- jPaste(tmp$ORF[indexVec[1]],"~",tmp$ORF[indexVec[2]])
  }
  enRanking
}


d2
convertToEdgeNameRanking(d2)


## test <- function(a,b)  (a == b*floor(a/b) + a%%b)


