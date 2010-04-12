## Best run this way:  C:/Progra~1/R/R-2.9.1/bin/Rscript.exe make-slides.R
source("~/.Rprofile")
source("visualize.R")
source("mcmc.R")
##setwd("..")
library(igraph)




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
