##source("c:/Documents and Settings/Administrator/.Rprofile")
##setwd("C:/projects/sbm/R")
source(jPaste(SBM_PATH, "likelihood.R"))
library(plotrix)

## ranking: vector of edge names, in order of arrival
## row: the index of the row (run)
## plots rectangles
## output: none
plotHeatmap <- function(ranking,rankingBool,row,rowSize,makeText=FALSE,makeBalls=FALSE){
  jCat(ranking,row,rowSize)
  spacing <- 1
  for(i in 1:length(rankingBool)){
    if(rankingBool[i]){color <- "orange" }
    else{color <- "blue" }    
    rect(spacing*(i-1), (row-1)*rowSize, spacing*i, row*rowSize, col=color, border="white")
    if (makeText)
      text(spacing*(i-1), (row-0.5)*rowSize,ranking[i],pos=4)
    if (makeBalls){
      colorPair <- makeColorPair(ranking[i])
      jCat("color1 = ", colorPair[1], "; color2 = ", colorPair[2])
      draw.circle(spacing*(i-0.5), (row-2)*rowSize, 0.3, col=colorPair[1], border=colorPair[1])
      draw.circle(spacing*(i-0.5), (row-2.6)*rowSize, 0.3, col=colorPair[2],border=colorPair[2])  
      ##symbols(spacing*(i-0.5), (row-2)*rowSize,c(0.05),inches=0.0625,bg=colorPair[1],add=TRUE)
    }
  }
}

makeColorPair <- function(edgeName){
  fn <- function(i) moduleColor(match(getLetter(strsplit(edgeName,split="~")[[1]][i]),LETTERS))
  sapply(c(1,2),fn)
}

##runs: a list of edge rankings
##plots a "table"-style heatmap, in which:
## * each ranking corresponds to a row
## * the arrivals are shown from left(first) to right(last)
##output: none
plotRuns <- function(runs,title="edge ranking",makeText=FALSE,makeBalls=TRUE){
  n <- length(runs$arrivals)
  k <- length(runs$arrivals[[1]])
  plot(c(0,k),c(0,n),type="n",bty="n",xaxt="n",yaxt="n"
       ,xlab="",ylab="", main=title, asp=1)
  row <- 0
  for (ranking in runs$arrivals){
    row <- row+1
    rankingBool <- sapply(ranking, function(edgeName){
      node1 <- parseTilde(edgeName)[1]
      node2 <- parseTilde(edgeName)[2]
      (getLabel(node1,vList)==getLabel(node2,vList))})
    plotHeatmap(ranking,rankingBool,row,1,makeText,makeBalls)
  }
}


makeColorFn <- function(paletteName){
  ##paletteName <- "RdBu" ###################
  jCols <- brewer.pal(n = 3, name = paletteName)
  gColor <- colorRamp(rev(jCols), bias = 1, space = c("rgb"), interpolate = c("linear", "spline"))
  function(x) {
    epsilon <- 1e-10 ##centering to take care of silly numerical problems
    x <- (1 - epsilon) * x + epsilon * 0.5    
##  jCat("gColor(1) = ", gColor(1))
##  jCat("x = ", x);  jCat("x-1 = ", x-1); jCat("length(x) = ", length(x)); jCat("gColor(x) = ", gColor(x));
  rgb(gColor(x)/256)}
}

##
colRedBlue <- makeColorFn("RdBu")
colGreys <- makeColorFn("Greys")

##how to share plots when you have two plotting functions?

makeHeatmapRanking <- function(ranking, makeText=TRUE){
  Nodes <- truth$Nodes
  k <- length(Nodes)
  par(xaxt="n",yaxt="n")
  plot(c(-.25*k,k),c(0,k+1.5), type="n",xlab="",ylab="", bty="n", asp=1)
  ##text(0,k+1.5,"Edge ranking", pos=4)

  for (i in 1:k){
    dispText <- Nodes[i]
    text(-0.5,k-i+0.5,dispText)
    text(i-0.5,k+0.5,dispText)
    
    for (j in 1:k){
      ##get element whose name equals "i,j"
      cp <- match(jPaste(Nodes[i],"~",Nodes[j]), ranking)
      if(is.na(cp))
        cp <- match(jPaste(Nodes[j],"~",Nodes[i]), ranking)
      ##jCat("i=",i ,"  j=",j,"  cp = ", cp)
      if (!is.na(cp)){
        ##jCat("cp = ", cp)
        color <- colGreys(cp/(ne+1)) ##colGreys(1 - cp/ne)
        rect(i-1,k-j+1,i,k-j,col=color, border="white")
        if(makeText){
          text(i-0.5,k-j+0.5,cp,col="black")
        }
      }
    }
  }
}



## 'proportion' must be set

#makes a heatmap, given the hashtable 'post'
#f is the function plotted
#k is the maximum number of models shown 
makeHeatmapPostRI <- function(post,f,k,makeText=TRUE, trueStructure=NULL, computeConsensus=FALSE){
  post <- sort(post, decreasing = TRUE)
  kFound <- length(post)
  jCat("kFound = ", kFound)
  
  ## k is the number of structures displayed; kFound is the number in topk
  
  if(k>kFound){
    jCat("k is too big. Setting k to ", length(post),".")
    k <- kFound 
  }
  labs <- names(post[1:k]) ##labelings

  
  prop <- c()
  prop[1:kFound] <- proportion[1:kFound]
  
  if(is.na(match(truth$structure,labs))){   ##if the truth isn't among the topk
    labs[k+1] <- truth$structure
    prop[k+1] <- exp(objective(cz(truth$structure)))/totalMass ##not adding to 1 currently, but close.
    jCat("cz(trueStructure) = ", cz(truth$structure))
    jCat("prop[k+1] = ", prop[k+1])
    k <- k+1 ##show the truth
  }

  ##add random junk labelings
  n <- sum(sMod)
  for (i in seq_along(rep(0,2))){
    labs[k+1] <- Reduce(jPaste,randomLabeling(n))
    k <- k+1
  }

  jCat("k = ", k)
  
  
  par(xaxt="n",yaxt="n")
  plot(c(-0.6*k,k),c(0,k+1.5), type="n", xlab="", ylab="", xaxt="n",yaxt="n",bty="n", asp=1)
  write(file="totalMass.tex", signif(totalMass,3))
  
  text(0,k+0.5,"posterior proportion, out of 1000",cex=0.6,pos=2)
  text(0,k+0.5,"Rand Index, in %", pos=4)

  ##ri for consensus computation
  ri <- matrix(nrow=kFound,ncol=kFound)
  for (i in 1:kFound)
    for (j in 1:kFound)
      ri[i,j] <- f(cz(names(post[i])), cz(names(post[j])))

  ##ri for displaying
  riDisplay <- matrix(nrow=k,ncol=k)
  for (i in 1:k)
    for (j in 1:k)
      riDisplay[i,j] <- f(cz(labs[i]), cz(labs[j]))

  
  minRi <- min(riDisplay) ##the minimum of the displayed RIs

  consensusIndex <- NULL
  if (computeConsensus){ ##find the one having the smallest squared average rand distance
    averageRandDistance <- function(index) weighted.mean((rep(1,kFound)-ri[index,1:kFound]), proportion[1:kFound])
    averageRandDistance(1)
    jCat("proportion = ", proportion)
    jCat("kFound = ", kFound)
    averageRIs <- sapply(1:kFound, averageRandDistance)
    jCat("averageRIs = ", averageRIs)
    consensusIndex <- which.min(averageRIs)
    write(labs[consensusIndex], file="consensus-RI.tex")

    write(jPaste("\"name\", \"consensus\", \"score\""), file="consensusResults.csv", append=TRUE)
    write(jPaste("randDist, ", labs[consensusIndex], ", ", randIndex(truth$structure, labs[consensusIndex])), file="consensusResults.csv", append=TRUE)
  }
  
  
  for (i in 1:k){ ##iterate over the displayed structures
    ##jCat("i = ", i , ", k = ", k, ", prop[i] = ", prop[i])
    rect(0,k-i,-10*prop[i],k-i+1,col="grey")
    dispText <- jPaste(10^3*round(prop[i],3), " : ",labs[i])
    text(0,k-i+0.5,dispText,pos=2)
    
    if(labs[i]==truth$structure){ ##boldface it
      text(0+0.03,k-i+0.5,dispText,pos=2)
      text(0,k-i+0.5+0.03,dispText,pos=2)
      text(0+0.03,k-i+0.5+0.03,dispText,pos=2)
    }

    if (computeConsensus && i==consensusIndex){
      text(0+0.03,k-i+0.5,dispText,pos=2, col="red")
      text(0,k-i+0.5+0.03,dispText,pos=2, col="red")
      text(0+0.03,k-i+0.5+0.03,dispText,pos=2, col="red")
    }
    
    for (j in 1:k){ ## painting rectangles in this row
      color <- colRedBlue((riDisplay[i,j]-minRi)/(1-minRi))
      rect(i-1,k-j+1,i,k-j,col=color, border="white")
    }
  }

  ##if truth not among the top k, add it below them.  
  if(makeText){
    for (i in 1:k){
      for (j in 1:k){
        ri <- f(cz(labs[i]), cz(labs[j])) ##randIndex
        text(i-0.5,k-j+0.5,100*round(ri,2),col="black")
      }
    }
  }
}


#makes a heatmap, given the hashtable 'post'
makeHeatmapColabeling <- function(colabelingProbs,makeText=TRUE, trueStructure=NULL){
  Nodes <- truth$Nodes
  ##edgeLabels <- names(colabelingProbs) ##edge labels (numbered like: 1,2)
  k <- length(Nodes)
  
  par(xaxt="n",yaxt="n")
  ##title <- "colabeling probabilities"
  ##  title <-  jPaste("true structure = ",formatVector(sMod), ", rtrue=", rtrue)
  ##subtitle <- jPaste("observed ranking = ", Reduce(function(a,b) jPaste(a,", ",b),ranking[1:20]),", ...")
  plot(c(-.25*k,k),c(0,k+1.5), type="n", xlab="",ylab="", bty="n", xaxt="n", yaxt="n", asp=1)
  ##text(k,k+1.5,subtitle,cex=0.5,pos=2)
  
  text(0,k+1.5,"Colabeling probability, in %", pos=4)

  for (i in 1:k){
    dispText <- Nodes[i]
    text(-0.5,k-i+0.5,dispText)
    text(i-0.5,k+0.5,dispText)
    
    for (j in 1:k){
      ##get element whose name equals "i,j"
      cp <- colabelingProbs[jPaste(i,",",j)] ##getByName(jPaste(i,",",j),colabelingProbs)
      if(i==j)
        cp <- 1
      if(is.na(cp))
        cp <- colabelingProbs[jPaste(j,",",i)] ##getByName(jPaste(i,",",j),colabelingProbs)
      jCat("i=",i ,"  j=",j,"  cp = ", cp)
      if (!is.na(cp)){
        jCat("cp = ", cp)
        color <- colRedBlue(cp)
        rect(i-1,k-j+1,i,k-j,col=color, border="white")
          if(makeText){
            text(i-0.5,k-j+0.5,100*round(cp,2),col="black")
          }
      }
    }
  }   
}




##make within edges orange
##make between edges blue


moduleColor <- function(moduleIndex){
  l <- c("#ffcccc","#ccffcc","#ccccff","#ffccff","#ffffcc","#ccffff", "#cccccc")
  l[moduleIndex]
}

isWithin <- function(edgeName){
  print(edgeName)
  print(length(edgeName))
  spl <- strsplit(edgeName,"~")[[1]]
  node1 <- spl[1]; node2 <- spl[2]
  label1 <- getLabel(node1,vList); label2 <- getLabel(node2,vList)
  (label1==label2)
}

getColor <- function(edgeName){
  ifelse(isWithin(edgeName),"orange","blue")
}


## graphNEL
visualizeStructure <- function(gr, title="complete graph"){
##  gr <- kn
  width <- 20
  ##edgeFoo <- do.call(rbind, strsplit(edgeNames(kn), split = "~"))
  ##jFrom <- edgeFoo[,1]
  ##jTo <- edgeFoo[,2]
  ##vList <- lapply(seq_len(M), function(m) { return(paste(LETTERS[m], seq_len(sMod[m]), sep = "")) })  
  
  blockLabels <- rep(seq_len(M), sMod) ##for sMod=[2,3,4]  you get [1 1 2 2 2 3 3 3 3]
  names(blockLabels) <- nodes(gr)
  
  nAttrs <- list(fillcolor=sapply(blockLabels,moduleColor))
  ns <- nodes(gr)
  nAttrs <- lapply(nAttrs, function(x) {
    names(x) <- ns
    x
  })
  ##eAttrs <- list(label = unlist(edgeData(kn, from = jFrom, to = jTo, attr = "weight")))
  ##eAttrs <- lapply(eAttrs, function(x) {
  ##  names(x) <- edgeNames(kn)
  ##  x
  ##})
  attrs <- list(graph = list(rankdir = 'LR'))
  subGList <- lapply(seq_len(M), function(m) {return(list(graph = subGraph(vList[[m]], gr)))})

  gRl <- layoutGraph(gr, ##subGList = subGList,
                     attrs = attrs)
  nodeRenderInfo(gRl) <- list(fill = nAttrs$fillcolor, fontsize = 8)
  ##eAttrs$col[1:numEdges(kn)] <- "white"  
  renderGraph(gRl, graph.pars = list(graph = list(main = title)))
}



##visualize a single run (and produce PDFs)
##shows graph + "heatmap" representation
##run is a sequence of edgeNames
visualizeRun <- function(ranking){
  width <- 20
  edgeFoo <- do.call(rbind, strsplit(edgeNames(kn), split = "~"))
  jFrom <- edgeFoo[,1]
  jTo <- edgeFoo[,2]
  blockLabels <- rep(seq_len(M), sMod)
  names(blockLabels) <- vVect
  nAttrs <- list(fillcolor=sapply(blockLabels,moduleColor))
  nAttrs <- lapply(nAttrs, function(x) {
    names(x) <- nodes(kn)
    x
  })
  eAttrs <- list(label = unlist(edgeData(kn, from = jFrom, to = jTo, attr = "weight")))
  eAttrs <- lapply(eAttrs, function(x) {
    names(x) <- edgeNames(kn)
    x
  })
  attrs <- list(graph = list(rankdir = 'LR'))
  subGList <- lapply(seq_len(M), function(m) {return(list(graph = subGraph(vList[[m]], kn)))})
  gRl <- layoutGraph(kn, subGList = subGList, attrs = attrs)
  nodeRenderInfo(gRl) <- list(fill = nAttrs$fillcolor, fontsize = 8)
  eAttrs$col[1:numEdges(kn)] <- "white"  
  
  ##there is some code to handle the special case in which i==0
  for(i in 0:numEdges(kn)) {
    jCat("i=",i)
    jMain <- paste("visible edges have rank <=", i)
    edgeName <- ranking[i]
    if(i>=1){ 
              color<- getColor(edgeName); eAttrs$col[edgeName] <- color}
    edgeRenderInfo(gRl) <- eAttrs
    renderGraph(gRl, graph.pars = list(graph = list(main = jMain)))
    for(j in 1:i){ ##it would be nice if I could avoid re-plotting
      if(i<1) break
      print(color)
      color <- getColor(ranking[j])
      rect(width*j,0,width*(j+1),30,col=color,border="white")
    }
##    dev.print(pdf, paste(jPaste(path,"/"), i + 100, ".pdf", sep = ""), width = 6, height = 6)
  }
}


plotStochasticSearchProgress <- function(ssRun){
  mass <- ssRun$mass
  nModels <- ssRun$nModels ##number of models visited
  
  plot(1:config$nIter, mass, col="red", type="l", xlab="", ylab="")
    ##show the restarts
  for(i in 1:(config$nIter/config$nIterPerRestart))
    abline(v=i*config$nIterPerRestart, col="grey")
  abline(v=0)
  abline(h=0)
  axis(2,col="red")
  par(new=TRUE)  
  plot(1:config$nIter, nModels, type="l", col="black", main=jPaste(" # of models = ",max(nModels),"    total mass = ",signif(max(mass),3)), xlab="iteration number", ylab="black: number of models seen     red: mass seen", yaxt="n")
  axis(4,col="black")

}
