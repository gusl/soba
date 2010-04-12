library(graph)
##library(Rgraphviz)

##initializing the true model
init <- function(sMod){
  sMod <<- sMod
  M <<- length(sMod)                       # number of modules
  (nv <<- sum(sMod))                         # number of nodes
  (ne <<- choose(nv, 2))                     # number of edges
  vList <<- lapply(seq_len(M), function(m) { return(paste(LETTERS[m], seq_len(sMod[m]), sep = "")) })
  names(vList) <<- LETTERS[1:M]
  (vVect <<- unlist(vList)) ## vVect: vector of all nodes (ignores modules)
  Nodes <<- unlist(vList, use.names=FALSE)
  numLabels <<- length(vList)
  
  ##all edges are appearing twice!
  ##required by the undirected graph data structure!
  eList <<- lapply(seq_along(vVect), function(i) { return(list(edges = (seq_along(vVect)[-i])))})
  
  names(eList) <<- vVect
  (kn <<- new("graphNEL", nodes = vVect, edgeL = eList))
  (gr2 <<- new("graphNEL", nodes = vVect))

  ##set up labels
  for (i in 1:length(vList)){ names(vList[[i]]) <<- rep(LETTERS[i], sMod[i]) }

  availableLetters <<- setdiff(LETTERS,LETTERS[1:M])
}

numWithinEdgesVec <- function(vec){
  ##jCat("vec = ", vec)
  newSMod <- computeSModVec(vec)
  sum(sapply(newSMod, function(x) choose(x,2)))
}


##splits the string using the first Tilde
parseTilde <- function(s){
  ##jCat("s = ")
  ##print(s)
  unlist(strsplit(s,"~")) }

## Q: can I get this data structure to give me a simple list of edges?
## A: edgeNames(gR)

####vVect <- unlist(vList)

##input: node name
##output: letter
getLetter <- function(s){
  substr(s,1,1)
}


##input: edge name
##output:
##s: String. If they start with the same letter, return r, otherwise 1
computeWeight <- function(s,r){
  node1Str <- parseTilde(s)[1]
  node2Str <- parseTilde(s)[2]
  ifelse (getLetter(node1Str)==getLetter(node2Str), r, 1)
}

## use strsplit("~")

##ToDo: rename this to computeWeights
##computes the weight for all edges in kn
weights <- function(r) { ##vectorize this
  wts <- c()
  edgeNams <- edgeNames(kn)
  for (i in 1:ne) {   ##iterate through edges
      wts[i] <- computeWeight(edgeNams[i],r)
  }
  return(wts)
}

normalize <- function(ws){
  ws / sum(ws)
}

##sample edge arrivals

## return arrivals, arrivalTypes: R does not allow side-effects
generate <- function(r) {
  arrivals <- sample(edgeNames(kn), ne, replace=FALSE, prob=normalize(weights(r)))
  list(arrivals=arrivals)
}

##input: a vector with ranking of "W"s/TRUE (withins) and "B"s/FALSE (between), and their numbers
##output: log-likelihood of this ranking
loglik <- function(arrivalTypes,Ws,Bs,r) { ##Ws: # of W remaining
  ##  print("entered loglik")
  stopifnot(Ws==sum(arrivalTypes))
  sum <- 0
  num <- NULL
  for (i in 1:length(arrivalTypes)){
    ##names(arrivalTypes[i]) <- ""
    if(arrivalTypes[i]==TRUE) num <- r else num <- 1
    sum <- sum + log(num / (r*Ws + Bs))
    ##sum <- sum + log(ifelse(arrivalTypes[i]==TRUE,r,1) / (r*Ws + Bs))
    if(arrivalTypes[i]==TRUE){ Ws <- Ws-1 } else{ Bs <- Bs-1 } ##decrement    
  }
  stopifnot(Ws==0)
  stopifnot(Bs==0)  
  sum
}


getLabel <- function(node,vList){
  ##jCat("entered getLabel")
  ##jCat("node = ",node)
  ##jCat("vList = "); print(vList)
  coordinates <- c()
  ##find node in vList
  for(i in 1:length(vList)){
    for(j in 1:length(vList[[i]])){
      if (vList[[i]][j]==node){
        coordinates <- c(i,j)
        break
      }
    }
  }
  stopifnot(coordinates!=c()) ##node not found in vList
  names(vList[[coordinates[1]]])[coordinates[2]]
}

vlistCoordinates <- function(vList,node){
  coordinates <- c()
  ##find node in vList
  for(i in 1:length(vList)){
    for(j in 1:length(vList[[i]])){
      if (vList[[i]][j]==node){
        coordinates <- c(i,j)
        break
      }
    }
  }
  coordinates
}


##returns a function
##for example, try isWithinBlock(mergings[[1]])("A1~C2")


isWithinBlockVec <- function(vec){
  nodes <- unlist(vList)
  h <- hash(keys=nodes,values=1:length(nodes)) ##efficiency: pull this out
  return(function(edgeName){
    ##jCat("edgeName = ", edgeName)
    node1 <- parseTilde(edgeName)[1]
    node2 <- parseTilde(edgeName)[2]
    node1Index <- h[[node1]]
    node2Index <- h[[node2]]
    (vec[node1Index]==vec[node2Index])
  })
}

isWithinBlock <- function(labeling, index1, index2){
  (labeling[index1]==labeling[index2])
}


##the order does not matter

computeSModVec <- function(vec){
  ##jCat("vec =", vec)
  h <- hash(keys=LETTERS, values=1:26)
  histogram <- rep(0,26)
  ##count the As, Bs, etc.
  for(i in 1:length(vec)){
    histogram[h[[vec[i]]]] <- histogram[h[[vec[i]]]] + 1
  }
  ##jCat("histogram = ", histogram)
  stopifnot(sum(histogram)>0)
  histogram[histogram!=0]
}

##computeSModVec(cz("CAABABCC"))


##l contains the possible values of the entries of v
computeHistogram <- function(v,l){
  histogram <- rep(0,length(l))
  names(histogram) <- l
  for(i in 1:length(v)){
    index <- match(v[i],l)
    histogram[index] <- histogram[index]+1
  }
  histogram
}


##should we name the nodes as numbers?
loglikVec <- function(ranking,labeling,r,loglikFun=loglik){
  ##jCat("loglikVec: labeling=", labeling, "      ranking = ", ranking)
  Ws <- numWithinEdgesVec(labeling)
##  jCat("Ws = ",Ws)
  Bs <- ne - Ws
  isWB <- isWithinBlockVec(labeling)

  binVector <- sapply(ranking, isWB)
  ##jCat("binVector = ")
  ##print(binVector)
  ##jCat(length(binVector))
  ll <- loglikFun(binVector,Ws,Bs,r)
  ##names(ll) <- ""
  ll
}

##loglikVec(ranking,cz("AABBCC"),10)

##represent as
##* lists of vectors, or
##* vector of labels


##number of splits for a block of size x
numSplit <- function(x) { 2^(x-1) - 1 }

##number of partitions in a set of size n
## 3 : 3  choose(3,1)
## 4 :    choose(4,1) + choose(4,2)
##A set of size n can be split in (2^n-2)/2 ways

##pass alt_sMod directly

##input: 
##output: split the i-th module 
splitBlocks <- function(sMod, index){
  modSize <- sMod[index]
  c(sMod[-index], 1, modSize - 1)
}


##put new merged block in front
mergeBlocks <- function(sMod, index1, index2){
  c(sMod[index1]+sMod[index2],sMod[-c(index1,index2)])
}


##randomly generate rankings
##input: number of runs n
##output: n rankings (coarsened to edge-type)
doExpt <- function(r,nRuns){
  ll <- list()
  arrivalTypes <- c()
  arrivals <- list()
  for (i in 1:nRuns){
    gen <- generate(r)
    (arrivals[[i]] <- gen$arrivals)
    ##(arrivalTypes[[i]] <- gen$arrivalTypes)
  }
  list(arrivals=arrivals)
} ##STOPPED PASSING arrivalTypes

generateMerge <- function(vList,i,j){
  newLetter <- availableLetters[1]
  names(vList[[i]]) <- rep(newLetter,sMod[i])
  names(vList[[j]]) <- rep(newLetter,sMod[j])
  vList
}

generateMergings <- function(vList){ ##generates mergings of the true model
  mergings <- list(); i <- 0
  for(col in columns(combn(M,2))){
    i <- i+1
    mergings[[i]] <- generateMerge(vList,col[1],col[2]) ##add, not append
  }
  mergings
}

##
##'subset' contains the indices of the first set (which will be assigned letter1); the rest get letter2.
generateSplit <- function(vList,iMod,subset){
  newLetter1 <- availableLetters[1]; newLetter2 <- availableLetters[2]
  names(vList[[iMod]])[subset] <- newLetter1
  names(vList[[iMod]])[-subset] <- newLetter2
  vList
}
##generateSplit(vList,3,c(1,3))

##all2Partitions
generateSplits <- function(vList){
  splits <- list(); i <- 0
  for (iMod in 1:length(vList)){
    splitsList <- all2Partitions(1:sMod[iMod]) ##1:length(sMod)
    for (split in splitsList){
      i <- i+1
      splits[[i]] <- generateSplit(vList,iMod,split)
    }
  }
  splits
}

##ToDo:
##* regroup vList, after relabeling
##

##vList version
#generateRelabeling <- function(vList,node,newLabel){
#  coords <- vlistCoordinates(vList,node); iMod <- coords[1]; index <- coords[2]
#  names(vList[[iMod]])[index] <- newLabel
#  vList
#}



useTimeStamp <- FALSE

##input: rankings; optional: loglikFun
##output: for each ranking (row), the likelihood for each model (column)
computeLikelihoods <- function(runs,r,loglikFun=loglik){ ##ToDo: models
  trueLik <- list(); mergLiks <- list(); splLiks <- list()

  mergings <- generateMergings(vList)
  splits <- generateSplits(vList)

  for (i in 1:length(runs$arrivals)){
    if(useTimeStamp) jCat("i = ",i, " ", timestamp()) ##timeCat: print + timestamp
    ranking <- runs$arrivals[[i]]
    trueLik[[i]] <- loglikVlist(ranking,vList,r,loglikFun=loglikFun)
    mergLiks[[i]] <- lapply(mergings, function(model) loglikVlist(ranking,model,r,loglikFun=loglikFun) )
    splLiks[[i]] <- lapply(splits, function(model) loglikVlist(ranking,model,r,loglikFun=loglikFun) )
  }
  list(truthLik=trueLik,mergLiks=mergLiks,splLiks=splLiks)
}


##concatenation issue!


computeLikelihoodsRelabelings <- function(runs,r,loglikFun=loglik){
  trueLik <- list(); relLiks <- list(); set1Liks <- list(); set2Liks <- list()
  trueW <- numWithinEdges(vList)
  relabelings <- generateRelabelings(vList)
  parti <- partitionModels(relabelings,trueW)

  for (i in 1:length(runs$arrivals)){
    if(useTimeStamp) jCat("i = ",i, " ", timestamp()) ##timeCat: print + timestamp
    ranking <- runs$arrivals[[i]]
    trueLik[[i]] <- loglikVlist(ranking,vList,r,loglikFun=loglikFun)
    set1Liks[[i]] <- lapply(parti$set1, function(model) loglikVlist(ranking,model,r,loglikFun=loglikFun))
    set2Liks[[i]] <- lapply(parti$set2, function(model) loglikVlist(ranking,model,r,loglikFun=loglikFun))
  }
  list(truthLik=trueLik,mergLiks=set1Liks,splLiks=set2Liks)
}


illustrate <- function(ranking){
  subGList <- lapply(seq_len(M), function(m) {
    return(list(graph = subGraph(vList[[m]], kn)))})
  edgeNames(kn)
  edgeFoo <- do.call(rbind, strsplit(edgeNames(gR), split = "~"))
  jFrom <- edgeFoo[,1]
  jTo <- edgeFoo[,2]

}

##sMod <- c(1,2,2,3,5,7)

##input: index in the list of merges
##output: sizes of original blocks before they were merged
lookupMerge <- function(vList,mergeIndex){
  sMod <- computeSMod(vList)
  pair <- unlist(columns(combn(1:length(sMod),2))[mergeIndex])
  c(sMod[pair[1]], sMod[pair[2]])
}

sigd <- function(r,n) floor(10^(n)*r)/10^(n) ##round down


convert <- function(lls,nRuns){
  mergBest <- c(); splitBest <- c(); mergBestIndex <- c(); splitBestIndex <- c(); truthLik <- c()
  for (i in 1:nRuns){
    mergBest[i] <- max(unlist(lls$mergLiks[[i]])) - lls$truthLik[[i]]
    mergBestIndex[i] <- which.max(unlist(lls$mergLiks[[i]]))
    splitBest[i] <- max(unlist(lls$splLiks[[i]])) - lls$truthLik[[i]]
    splitBestIndex[i] <- which.max(unlist(lls$splLiks[[i]]))
  }
  rang <- range(c(mergBest,splitBest))
  nMerges <- length(lls$mergLiks[[1]]); nSplits <-  length(lls$splLiks[[1]])
  
  pcent <- function(v) { s <- sigd(sum(v)/nRuns,3);  jPaste(100*s,"%") }
  txt <- jPaste(nRuns," runs; m>t: ",pcent(mergBest>0),"; s>t: ",pcent(splitBest>0),"; m>s: ",pcent(mergBest>splitBest), "; t wins: ", pcent((mergBest<0)&(splitBest<0)))
  jCat(txt)
  ##ToDo: truth wins %
  
  list(nRuns=nRuns,nMerges=nMerges,nSplits=nSplits,mergBest=mergBest,splitBest=splitBest,range=rang,text=txt)
}

formatVector <- function(sMod)
  jPaste("[",Reduce(function(a,b) {jPaste(a,",",b)},sMod),"]")

preparePlot <- function(r,sMod,runs,s){
  plotName <- jPaste("bests: merge vs split (r=",r,", sMod=[",Reduce(function(a,b) {jPaste(a,",",b)},sMod),"])")
  ##how often is merge>truth, split>truth, merge>split
  jCat("s$range = ",s$range)
  plot(s$range,s$range,xlim=s$range,ylim=s$range,xlab=jPaste("ll(best merge) - ll(truth), out of ",s$nMerges),ylab=jPaste("ll(best split) - ll(truth), out of ",s$nSplits),main=plotName,type="n", asp=1)
  jCat("---1---")
  plotName
}

##pre: there is a plot present
plotPoints <- function(mergBest,splitBest,color){
  points(mergBest,splitBest,col=color)
  abline(a=0,b=1); abline(v=0); abline(h=0)
}

writePDF <- function(plotName){
  fileName <- jPaste(gsub(":","-",plotName),".pdf")
  jCat("Writing to file: ",fileName)
  dev.print(pdf,fileName, width = 6, height = 6)
}


##compare the truth against every split and every merge
oct06 <- function(r,sMod=sMod,nRuns=nRuns,s=s){
  if(!is.null(s)){
    sMod <- s$sMod
    runs <- s$runs1
    lls <- s$lls
  }
  else{ ##i.e. didn't pass 's'
    stopifnot(!is.null(sMod))
    sMod<<-sMod ##does that make it global
    init(sMod)
    jCat("Running experiments...")
    runs <- doExpt(r,nRuns)
    jCat("Computing likelihoods...")
    lls <- computeLikelihoods(runs,r)
  }
  s <- convert(lls,nRuns)
  plotName <- preparePlot(r,sMod,runs,s)
  plotPoints(s$mergBest,s$splitBest,"red")  ##r=10
  ##plotPoints(,"blue") ##same runs, r=5
  title(sub=s$text)
  writePDF(plotName)
  
  list(sMod=sMod,runs=runs,lls=lls)
}


nov04 <- function(rtrue,r,sMod=sMod,nRuns=nRuns,s=s){
  if(!is.null(s)){
    sMod <- s$sMod
    runs <- s$runs
    lls <- s$lls
  }
  else{ ##i.e. didn't pass 's'
    stopifnot(!is.null(sMod))
    sMod<<-sMod ##does that make it global
    init(sMod)
    jCat("Running experiments...")
    runs <- doExpt(rtrue,nRuns)
    jCat("Computing likelihoods...")
    ##for (i in 1:length(rs))
    lls1 <- computeLikelihoodsRelabelings(runs,r)
    ##lls2 <- computeLikelihoodsRelabelings(runs,rs[2])
    ##lls[[2]] <- computeLikelihoodsRelabelings(runs,rs[2])
  }
  s1 <- convert(lls1,nRuns)
##  s2 <- convert(lls2,nRuns)
  preparePlot(r,sMod,runs,s1)
  plotName <- jPaste("rel-B=",formatVector(sMod), ",rtrue=",rtrue,"r=",r)
  plotPoints(s1$mergBest,s1$splitBest,col=jCols[1])
##  plotPoints(s2$mergBest,s2$splitBest,col=jCols[2])
  title(sub=s1$text)
  writePDF(plotName)
  
  list(sMod=sMod,runs=runs,lls=lls1)
}


##numWithinEdgesVec(cz("AABBCC"))

##divide the relabelings into two: numWithinEdges greater than W
partitionModels <- function(models,W){
  finer <- list()
  rest <- list()
  fIndex <- 1; rIndex <- 1
  for (i in 1:length(models)){
    if(numWithinEdges(models[[i]])>W){
      finer[[fIndex]] <- models[[i]]
      fIndex <- fIndex+1
    }
    else{
      rest[[rIndex]] <- models[[i]]
      rIndex <- rIndex+1
    }
  }
  list(set1=finer,set2=rest)
}

##jitterPlot()



library(RColorBrewer) ##display.brewer.all(type = "qual")
jCols <- brewer.pal(n = 8, name = "Dark2")

getColor <- function(i) jCols[i]


##simulate using rtrue
##plot likelihoods for rs
oct27 <- function(rtrue,sMod=sMod,nRuns=nRuns,s=s,rs,loglikFunName="loglik"){
  loglikFun <- eval(parse(text=loglikFunName))
  jCat("using ", loglikFunName)
  if(!is.null(s)){
    sMod <- s$sMod
    runs <- s$runs
    lls <- s$lls
  }
  else{ ##i.e. didn't pass 's'
    stopifnot(!is.null(sMod))
    sMod<<-sMod ##does that make it global
    init(sMod)
    jCat("Running experiments...")
    runs <- doExpt(rtrue,nRuns)
    jCat("Computing likelihoods...")
    lls <- list()
    for (i in 1:length(rs))
      lls[[i]] <- computeLikelihoods(runs,rs[i],loglikFun=loglikFun)
  }

  fileName <- list()
  for (i in 1:length(rs)){
    jCat(rs[i])
    s[[i]] <- convert(lls[[i]],nRuns)
    fileName[[i]] <- jPaste("rtrue=",rtrue,",r=",rs[i],",sMod=",formatVector(sMod),",rs=",formatVector(rs),",score=",loglikFunName)
    jCat(fileName[[i]])
    write(s[[i]]$text,file=jPaste(fileName[[i]],".txt"))
  }

  preparePlot(rtrue,sMod,runs,s[[1]]) ##create range argument?

  for (i in 1:length(rs))
    plotPoints(s[[i]]$mergBest,s[[i]]$splitBest,getColor(i))
  
  writePDF(jPaste(fileName[[1]]))
  
  list(sMod=sMod,runs=runs,lls=lls)
}




##input: single run
##for sMods, the search space is too big
##so we consider random splits and merges 
##Gustavo - Oct 26
randomSearch <- function(runs,sMod=sMod,nSplits,nMerges){
  nRuns <- length(runs$arrivals)
  totalMerges <- choose(length(sMod),2)
  totalSplits <- sum(numSplit(sMod))
  jCat(totalMerges, " merges, ", totalSplits, " splits")
  
  ##sample without replacement from the set of splits

  mergeIndices <- sample(1:totalMerges,nMerges,replace=FALSE)
  splitIndices <- sample(1:totalSplits,nSplits,replace=FALSE)

  mergings <- generateMergings(vList)[mergeIndices]
  splits <- generateSplits(vList)[splitIndices]

  for(i in 1:nRuns){
    mergeLL[[i]] <- sapply(mergings, function(model) loglikVlist(runs$arrivals[[i]], model))
    splitLL[[i]] <- sapply(splits, function(model) loglikVlist(runs$arrivals[[i]], model))
    truthLL[[i]] <- loglikVlist(runs$arrivals[[i]], vList)
    ##return the merge/split/truth as a block structure
  }
  
  list(mergings=mergings, splits=splits,
       truthLik=truthLL,mergLiks=mergeLL,splLiks=splitLL)
}

makeList <- function(v){
  res <- list()
  for(i in 1:length(v)){
    res[[i]] <- v[i]
  }
  res
}

##mergLiks, splLiks

##sMod <- c(1,2,3,3,3,3,4,5,5,7,9)
##init(sMod)
##r <- 10
##run <- doExpt(10,3)
##s <- randomSearch(runs,sMod,50,50)
##plotAndWritePDF(sMod,run,s)



getPdfs <- function(l){
  res <- c()
  for(elt in l){
    if (elt!=gsub(".pdf","@",elt)){ res <- c(res,elt) }
  }
  res
}

##save object

##IDEAS:
##* use color to indicate the magnitude of truthLik?  freakish rankings are red
##* label splits and merges, with the sizes of the two subsets that got merged or split.




##ToDo: do more experiments to *increase* an existing data set
##
sep30 <- function(runs=NULL, nRuns=1){ ##optional argument: set of runs
  if (is.null(runs)){
    runs <- doExpt(r,nRuns)
  }
  
  liks <- computeLikelihoods(runs,r)
  
  for (i in 1:length(runs$arrivals)){
    truthLik <- liks$truthLik[[i]]
    mergLiks <- unlist(liks$mergLiks[[i]])
    splLiks <- unlist(liks$splLiks[[i]])

    jCat("RUN #",i)
    altModelsName <- "mergings"
    jCat("Comparing the true model against ", altModelsName, ".")
    jCat("The true model scores behind ", sum(truthLik < mergLiks), " models, out of ", length(mergLiks), ".")
    jCat()
    altModelsName <- "splits"
    jCat("Comparing the true model against ", altModelsName, ".")
    jCat("The true model scores behind ", sum(truthLik < splLiks), " models, out of ", length(splLiks), ".")
  }
    list(runs=runs,liks=liks)
}

