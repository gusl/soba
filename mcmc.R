source(jPaste(SBM_PATH, "likelihood.R"))
library(hash)
library(RColorBrewer)

##traverses 'vec' and returns the unique characters in the order in which they are seen
labelOrder <- function(vec){
  count <- 0
  seen <- c()
  for (i in 1:length(vec)){
    c <- vec[i]
    if (!(c %in% seen)){
      count <- count+1
      seen[count] <- c
    }
  }
  seen
}
##labelOrder(c())


##permute the labels so that the order of first-arrival matches alphabetical order
permuteLabels <- function(vec,order){
  ##first, convert to numbers
  h <- hash(keys=order, values=LETTERS[1:length(order)])
  sapply(vec, function(i) h[[i]])
}

##
## find relabeling of 'vec' so that
normalForm <- function(vec){
  order <- labelOrder(vec)   ##observe the order. That gives the desired permutation.
  permuteLabels(vec,order)
}

##we use flat prior, i.e. we just use the likelihood ratio
##use a symmetric proposal

##randomly pick a node
##randomly pick a new label for it
##compute lik-ratio
##full conditional

randomNode <- function(){
  node <- Nodes[ceiling(length(Nodes)*runif(1))]
  node
}

randomNodeIndex <- function(){
  ceiling(length(Nodes)*runif(1))
}

##change a node to a new label always has same probability regardless of which label.
##I can bring it to normal form after every step

##requires 'labels' to be declared;  take it as an argument?
randomLabel <- function(){
  Labels <- truth$Labels
  Labels[ceiling(length(Labels)*runif(1))]
}

randomLabeling <- function(n){
  sapply(1:n, function(...) randomLabel())
}



##naive implementation: compute the likelihood on all label
##better implementation: if we knew the sum of these, i.e. the marginal.
sampleNewLabel <- function(node, sample0){  
  models <- sapply(Labels, function(x){})
  probs <- sapply(models, loglik)
  totalProb <- sum(probs)
}


## we can also exploit the structure of the likelihood: all that matters is 


equal <- function(l1,l2){
  Reduce(`&&`,names(unlist(l1))==names(unlist(l2)))
}


##relabels a single entry
generateRelabeling <- function(vec,nodeIndex,newLabel){
  vec[nodeIndex] <- newLabel
  vec
}


##uniform

##k: number of nodes potentially relabeled
gProposal <- function(state0, k){
  state <- state0
  for (i in 1:k)
    state <- generateRelabeling(state, randomNodeIndex(), randomLabel())
  state
}

gProposalFixedNLabels <- function(state0,k,nLabels){
  while(TRUE){ ## (since there is no do-while / do-loop)
    proposedState <- gProposal(state0,k)
    if (countLabels(proposedState)==nLabels) break ##if wrong number, propose again
  }
  proposedState
}

countLabels <- function(s) length(unique(cz(s)))

########################################## convert to vec ###

##nLabels: if not specified, it's free to vary.
mhNextSample <- function(proposal,state0, objective, nLabels=NULL){
  ##rr <- 0 ##go into the loop at least once

  proposedState <- proposal(state0) ##random
  
  objOld <- objective(state0)
  objNew <- objective(proposedState)
  ##  jCat("state = ", proposedState, "   objective = ",objNew)
  alpha <- exp(objNew - objOld) ##since we're dealing with the log of the probability
  rr <- min(1,alpha)
  ##  jCat("acceptance prob = ",rr)
  if(runif(1)<rr){ ##accept
    nextSample <- proposedState
    ##    jCat("accepted!")
  }
  else{ ##reject
    nextSample <- state0
    ##    jCat("rejected!")
  }
  nextSample
}



convertVListToVec <- function(vList){
  (nam <- names(unlist(vList)))
  vec <- sapply(nam, function(x) strsplit(x,"\\.")[[1]][2])
  names(vec) <- NULL
  vec
}


mh <- function(proposal, initial, objective, burnin, nSamples){
  beforeTime <- as.numeric(Sys.time())
  h <- hash()
  
  sample <- initial ##never counted

  if (burnin>1)
    for (i in 1:burnin-1) {##make sure that burnin > 1
      jCat("mh: sample=",sample)
      sample <- mhNextSample(proposal, sample, objective)      
    }
  jCat("mh: finished burn-in! From now on, it will count.")
  
  ##samples <- list()
  for (i in 1:nSamples){
    sample <- mhNextSample(proposal, sample, objective)

    sampleNormalized <- concat(normalForm(sample))
    ##jCat("mh: sample=",sample)
    ##samples[[i]] <- sampleNormalized
    ##jCat("mh: obj = ", objective(sample))
    ##print(sample)
    
    if(has.key(sampleNormalized,h))
      h[sampleNormalized] <- h[[sampleNormalized]] + 1
    else
      h[sampleNormalized] <- 1
  }
  totalTime <- as.numeric(Sys.time()) - beforeTime
  list(samples=h,runTime=totalTime)
}
##need to concat the samples into strings

## instead of counting visits, 'h' now stores the objective
##
## * which neighborhood? (i.e. "support of the proposal" if like MCMC)
## * explore entire neighborhood? Y/N
## * if no: explore like MCMC?
## * how many current states? [MCMC: 1]
## * when the space of models is small / peaked, pruning is not necessary: in MOSS-speak, cprime=0
##



##cz("AAB")
nbhd <- function(state,k){
##  if (length(state)==1)
##    state <- cz(state)
  S <- list()
  count <- 0
  for (i in 1:length(state)){
    for (j in 1:k){
      newState <- state
      newState[i] <- LETTERS[j]
      if (concat(newState)!=concat(state) &&
          (length(unique(newState))==k)){ ##at Jenny's request (18 May 2010)
        count <- count+1        
        S[[count]] <- concat(normalForm(newState))
      }
    }
  }
  unique(S) ##duplicates appear once we take the normal form
}


nbhd(cz("ABC"),3)

nbhd("ABC",3)


##quick improvement on mcmc: avoid staying in the high-scoring models:
## reverse simulated annealing: temperature gets higher over time
## tFunction:



##flattening the objective:
## if 50% of the moves are , move a notch
## pass a modified (flatter) objective to mhNextSample

scaleObjective <- function(obj,n) function(x) n*obj(x)

### we aim for a 50% rejection rate
### every 100 samples...
### if we get >70% rejection, flatten the objective 10-fold
### if we get <30% rejection, spiken the objective 10-fold

sSearchAdaptive <- function(proposal, initial, objective, nSteps){
  beforeTime <- as.numeric(Sys.time())
  h <- hash()
  sample <- initial ##never counted

  obj <- objective
  accepted <- 50
  for(round in 1:(nSteps/100)) {
    if (accepted/100 < .3){
      obj <- scaleObjective(obj,0.1)
      jCat("obj: flattening")
    }
    else if (accepted/100 > .7){
      obj <- scaleObjective(obj,10)
      jCat("obj: spikening")
    }
    else
      jCat("obj: not changing")

    prevSampleNormalized <- NULL
    accepted <- 0 ##for this round
    for (i in 1:100){
      sample <- mhNextSample(proposal, sample, obj)
      sampleNormalized <- concat(normalForm(sample))
      if(prevSampleNormalized==sampleNormalized){
        accepted <- accepted+1
      }
      h[sampleNormalized] <- 1 ##we don't care if it was there before
      prevSampleNormalized <- sampleNormalized
    }
  }
  totalTime <- as.numeric(Sys.time()) - beforeTime
  
  ##  jCat("found __ distinct structures.")
  list(samples=h,runTime=totalTime)
}


h1 <- hash()
h1["a"] <- 1
h2 <- hash()
h2["b"] <- 2
h3 <- hash()
h3["a"] <- 11
h3["z"] <- 26
union(list(h1,h2,h3))

##Q: how do I take the union of two hashtables?

##random restarts
sSearchWithRR <- function(proposal, objective, nSteps, nRestarts){
  ss <- list()
  totalHash <- hash()
  totalMass <- c()
  for(i in 1:nRestarts){
    initial <- randomLabeling(length(Nodes))
    ss[[i]] <- sSearch(proposal, initial, objective, nSteps)
    totalHash <- union(list(totalHash,ss[[i]]$samples))
    totalMass[i] <- postMass(totalHash,objective)
  }
  plot(1:nRestarts,totalMass, main=jPaste("nSteps = ", nSteps, "  nRestarts = ", nRestarts))  
  list(ss=ss,totalMass=totalMass)
}

cleanup_MHsearch <- function(h,maxObj,logNeglect,n){
  pile <- list()
  ks <- keys(h); vs <- values(h)
  count <- 0
  for (i in 1:length(vs)){
    if (vs[i] < maxObj - logNeglect){
      count <- count+1
      pile[count] <- list(ks[i],vs[i]) ##store the structure and the log-probability
      h[ks[i]] <- NULL
    }
  }
  save(file=jPaste("discard",n), pile)
  h
}


#### 
## 'pile' should contain keys AND values, not just keys...


##add up the mass of the discarded structures
accountForDiscard <- function(){
  ##find all files named "discard*"
  foo <- list.files(pattern="discard*", full.names = TRUE)

  bigPile <- hash() ##we are recovering it

  for (i in 1:length(foo)){ ##load them onto memory, put all models on the big hashtable
    load(file=jPaste("discard",i))
    discardPile <- pile
    if(length(discardPile)>0){
      for (j in 1:length(discardPile)){
        jCat("length of discardPile", length(discardPile))
        k <- discardPile[j][[1]] ##key
        v <- discardPile[j][[2]] ##value
        jCat(" k = ", k)
        jCat(" v = ", v)
        ##bigPile[k] <- v
      }
    }
  }
  mass <- sum(exp(bigPile))
  jCat("Total mass discarded = ", mass)
  mass
}




##no scaling
sSearch_MH <- function(proposal, initial, objective, nSteps, restart=NULL, logNeglect){
  ##neglect: if it's smaller than the max by a factor of 'neglect' or more, throw it out.
  
  nModels <- c(); mass <- c(); time <- c();
  beforeTime <- as.numeric(Sys.time())
  h <- hash()
  sample <- initial ##never counted
  maxObj <- objective(cz(sample))
  
##  obj <- scaleObjective(objective,0.001)
  for (i in 1:nSteps){
    jCat("i = ",i)
    if(!is.null(restart) && (i%%restart)==0){
      h <- cleanup_MHsearch(h,maxObj,logNeglect,i/restart)
      sample <- randomLabeling(length(Nodes))      
    }
    sample <- mhNextSample(proposal, sample, objective)

    sampleNormalized <- concat(normalForm(sample))
    if (objective(cz(sampleNormalized)) + logNeglect > maxObj){ ##add if it's not insignificant
      obj <- objective(cz(sampleNormalized)) ##we don't care if it was there before
      ##we don't bother with cleaning
      h[sampleNormalized] <- obj
      if (obj>maxObj)
        maxObj <- obj
    }
    nModels[i] <- length(keys(h))
    ##post <- sapply(keys(h), function(x) exp(objective(cz(x))))
    mass[i] <- sum(exp(values(h))) #keep track of post
    time[i] <- as.numeric(Sys.time()) - beforeTime
  }
  totalTime <- as.numeric(Sys.time()) - beforeTime
  
  list(samples=h,runTime=time,mass=mass,nModels=nModels)
}

##throw out models that score poorly
cleanup <- function(S,threshold){
  jCat("threshold = ", threshold)
    k <- keys(S); v <- values(S)
    for (i in 1:length(S)){
    if (v[i]<threshold){
      ##print(h)
      ##jCat("k[i] = ", k[i])
      S[[k[i]]] <- NULL
    }
  }
  S
}


## randomly pick one
## explore neighborhood
## throw out the ones
## 


##beforeTime <- NULL ##declare as global

sSearch_MOSS <- function(proposal, initial, objective, nSteps, restart=NULL, logNeglect){
  ## ToDo:
  ## get rid of 'proposal': that is a property of sSearch_MH

  beforeTime <- as.numeric(Sys.time())
  jCat("beforeTime = ", beforeTime)

  ## run greedy for 100 random restarts
  localMaxima <- hash()
  for (i in 1:1){
    jCat("--- Greedy maximization:  random restart #", i)
    initialModel <- concat(normalForm(randomLabeling(length(Nodes))))
    localMaximum <- greedyMaximize(objective, nbhd, initialModel)$incumbent
    if (!has.key(localMaximum,localMaxima))
      localMaxima[localMaximum] <- 1
    else
      localMaxima[localMaximum] <-  localMaxima[[localMaximum]] + 1
  }
 
  print(localMaxima)

  ## now... are there any local minima we wish to discard?

  peaks <- sapply(keys(localMaxima), objective)  
  names(peaks) <- keys(localMaxima)
  jCat("peaks = "); print(peaks)
  peaks <- sort(peaks, decreasing=TRUE)[1] ##only keep the top one
  jCat("after: peaks = "); print(peaks)
  
  ##cleanup(, max(heights)+log(0.01))
  
  ## run MOSS once for each
  mossRun <- NULL
  count <- 0
  for (m in names(peaks)){
    count <- count+1
    mossRun <- moss(m, objective, nSteps, restart=NULL, logNeglect, beforeTime)
  }
  
  jCat("mossRun = ")
  print(mossRun)

  list(samples=mossRun$samples,runTime=mossRun$runTime,mass=mossRun$mass,nModels=mossRun$nModels)
  
##   jCat("--1--")
##   mossRuns[1]$samples
##     jCat("--2--")
##   keys(mossRuns[1]$samples)
##     jCat("--3--")
  
##   ## merge the results
##   mergedSamples <- unique(c(keys(mossRuns[1]$samples), keys(mossRuns[2]$samples)))
##   jCat("mergedSamples = "); print(mergedSamples)
##   mergedObj <- objective(mergedSamples)
##   jCat("mergedObj = "); print(mergedObj)
##   mergedValues <- exp(mergedObj)
##   jCat("mergedValues = "); print(mergedValues)
##   mergedMass <- sum(mergedValues)
##   jCat("mergedMass = "); print(mergedMass)
}
  

greedyMaximize <- function(objective, neighborhood, initial){
  incumbent <- initial
  fmax <- objective(incumbent)
  while(TRUE){
    jCat("incumbent = ", incumbent)
    jCat("fmax = ", fmax)
    neighbors <- neighborhood(cz(incumbent),3)
    ##jCat("neighbors = ")
    ##print(neighbors)
    scores <- sapply(neighbors,objective)
    ##jCat("scores = ", scores)
    bestNeighborScore <- max(scores)
    
    ##argmax
    if(bestNeighborScore > fmax){
      fmax <- bestNeighborScore
      incumbent <- neighbors[[which.max(scores)]]
    }
    else break    
  }
  list(incumbent=incumbent, fmax=fmax)
}


fun <- function(model) sum(cz(model)=="A")


## greedyMaximize(fun, nbhd, "ABC")


moss <- function(initialModel, objective, nSteps, restart=NULL, logNeglect, beforeTime){  
  neighborhood <- function(model) nbhd(cz(model),truth$numLabels)
  logcprime <- log(cprime)
  jCat("logcprime = ", logcprime)
  maxIter <- 10000
  maxSizeS <- 1000
  S <- hash()
  unexploredModels <- hash() ##subset of S
  nModels <- c(); mass <- c(); incumbents <- c(); objectives <- c(); runTime <- c()

  incumbent <- initialModel
  initialObj <- objective(initialModel)
  S[initialModel] <- initialObj
  fmax <- initialObj
  unexploredModels[initialModel] <- 1
  
  for (i in 1:maxIter){
    jCat("MOSS: i = ", i)
    ##jCat("S = ")
    ##print(S)
    if(length(unexploredModels)>0){
      m <- sample(keys(unexploredModels),1) ##should not sample uniformly      

      ##isGood <- FALSE
      ##while(!isGood){ ##inefficient but correct. ToDo: sample directly from unexploredModels
      ##  jCat("length(S) = ", length(S))
      ##  m <- sample(keys(S), 1, prob=exp(values(S)))
      ##  jCat("m = ", m)
      ##  if (m %in% keys(unexploredModels)) isGood <- TRUE ##break ##only proceed if m is unexplored
      ##}
      jCat("We now are going to explore m = ", m)
    } else break
    neighbors <- neighborhood(m) ##do we really want truth$numLabels?
    jCat(" Evaluating ", length(neighbors), " neighbors.")
    for (mprime in neighbors){
      jCat("|  mprime = ", mprime)
      logpost <- objective(mprime)
      ##jCat("|  S = ")
      ##print(S)
      if ((logpost>logcprime+fmax) & (!mprime %in% keys(S))){
        unexploredModels[mprime] <- 1 ##add mprime to unexploredModels          
        S[mprime] <- logpost             ##add mprime to S          
        print("ACCEPTED")
        
        if (length(S) > maxSizeS) {}  ##do nothing  (##remove worst model)
        if (logpost>fmax){
          incumbent <- mprime
          fmax <- logpost
          S <- cleanup(S,logcprime+fmax)
        }
      }      
    }
    unexploredModels[m] <- NULL ##ToDo: mark as explored with a "0", instead of removing it
    
    nModels[i] <- length(S)
    mass[i] <- sum(exp(values(S))); jCat("mass[i] = ", mass[i])
    runTime[i] <- as.numeric(Sys.time()) - beforeTime; jCat("runTime[i] = ", runTime[i])
    incumbents[i] <- incumbent; jCat("incumbent = ", incumbent)
    objectives[i] <- fmax
  }
  jCat("MOSS: finished")
  jCat("MOSS: returning ", length(S), " models.")
  list(samples=S,runTime=runTime,mass=mass,nModels=nModels)
}




##explores the entire 'nbhd'
sSearch2 <- function(proposal, initial, objective, nSteps){
  beforeTime <- Sys.time()

  h <- hash()
  
  sample <- initial ##never counted
  
##  obj <- scaleObjective(objective,0.001)
  for (i in 1:nSteps){
    objectiveScaling <- 1/(10^(i/5))
    adjObjective <- scaleObjective(objective,objectiveScaling)
    sample <- mhNextSample(proposal, sample, adjObjective)
    sampleNormalized <- concat(normalForm(sample))    
    h[sampleNormalized] <- 1 ##we don't care if it was there before
  }
  totalTime <- Sys.time() - beforeTime
  list(samples=h,runTime=totalTime)
}





##start scaling the objective

postMass <- function(structures, objective){
  models <- keys(structures)
  post <- sapply(models, function(x) exp(objective(cz(x))))
  sum(post)
}



computeBinVector <- function(labeling){
  count <- 0
  bv <- c()
  for(i in 1:(length(Nodes)-1))
    for(j in (i+1):length(Nodes)){
      count <- count+1
      bv[count] <- ifelse(labeling[i]==labeling[j],1,0)
      names(bv)[count] <- jPaste(i , "," , j)
      ##jCat(i,",", j)
    }
  bv
}
##computeBinVector(cz("AAABBBCCCCC"))

##mixture : hash<model,probability>
##weighted average of the 0-1 vectors
computeColabelingProbs <- function(post) {
  models <- keys(ssRun$samples)
  (totalMass <- sum(post))
  probs <- post/totalMass
  ##stop sorting
  jCat("probs = ",probs)
  sumVector <- rep(0,ne)
  jCat(sumVector)
  for (i in 1:length(models)){
    binVector <- computeBinVector(cz(models[i]))
    ##jCat("models[i] = ", models[i], "  binVector = ", binVector, "  prob = ", probs[i])
    sumVector <- sumVector + binVector*probs[i]
  }
  sumVector
}



#plots the mass (and #models) for 2 methods, measured after every so many iterations
compare <- function(method1, method2, nSteps){

}

## AAABBB
## AABABB
## 
## rand = 
##
##
## AA AA AB BA BB BB
## 
## 

randIndex <- function(lab1,lab2){
  ##jCat("lab1=",lab1,"   lab2=",lab2)
  if (length(lab1)==1)
    l1 <- cz(lab1)
  else l1 <- lab1
  if (length(lab2)==1)
    l2 <- cz(lab2)
  else l2 <- lab2
  ##jCat("l1=",l1,"   l2=",l2)
  randIndexVec(l1,l2)
}

##concatenate the labels from the two
##considered as vectors
randIndexVec <- function(lab1,lab2){  
  n <- length(lab1)
  edgeIndices <- columns(combn(1:n,2)) ##pairs
  vector1 <- sapply(edgeIndices, function(indices) isWithinBlock(lab1,indices[1],indices[2]))
  vector2 <- sapply(edgeIndices, function(indices) isWithinBlock(lab2,indices[1],indices[2]))
  sum((vector1&vector2)|(!vector1 & !vector2))/length(edgeIndices)
}
##randIndex(cz("AABBCC"),cz("AAABCC"))

sigmoid <- function(x) exp(x)/(1+exp(x))
sigmoidFn <- function(mean, sd) function(x) sigmoid((x-mean)/sd)
sigmoidFn(1,1)(2)


##fr <- function(x) (sigmoidFn(.85,0.25)(x))*dnorm(x,mean=1,sd=.4)*1.2 ##/exp(1)
##fg <- function(x) (sigmoidFn(0.5,0.5)(x))*dnorm(x,mean=.5,sd=.3) ##/exp(1)
##fb <- function(x) (sigmoidFn(0.15,1)(x))*dnorm(x,mean=.15,sd=.3) ##/exp(1)
##
##fr <- function(x) pushup(3)((sigmoidFn(.8,0.25)(x))) ##*dnorm(x,mean=1,sd=.5)
##fg <- function(x) pushup(1)((sigmoidFn(0.6,0.5)(x))) ##*dnorm(x,mean=.5,sd=.5)
##fb <- function(x) sigmoidFn(0.4,1)(x) ##*dnorm(x,mean=.25,sd=.5)
##
##
##pushup <- function(p) function(x) 1-(1-x)^p
##
##fr <- function(x) ifelse(x<0.5, (x/0.5), 1)
##fg <- function(x) ifelse(x<0.5, (x/0.5), pushup(0.5) (1-1*(x-0.5)))
##fb <- function(x) ifelse(x<0.5, 1, pushup(0.5) (1-2*(x-0.5)))
##
##plot(c(0,1),c(0,1))
##t <- (0:100)/100
##points(t,fr(t))
##points(t,fg(t))
##points(t,fb(t))
##
##colF <- function(x) rgb(fr(x),fg(x),fb(x))
##
##colFull <- function(x) {
##  total <- fr(x) + fg(x) + fb(x)
##  rgb(fr(x)/total,fg(x)/total,fb(x)/total)
##}






getByName <- function(name,vec){
  index <- match(name,names(vec))
  jCat(index)
  if(is.na(index))
    res <- NULL
  else
    res <- vec[index]
  res
}



## is the truth visited? how often?

## what proportion of the posterior mass does the truth account for?

## sum of likelihoods gives



##'model': block structure
modelEntropy <- function(model){
  sMod <- computeSModVec(cz(model))
  entropy(sMod/sum(sMod))
}

##sum(vec) = 1, no 0s
entropy <- function(vec){
  total <- 0
  for (i in 1:length(vec)){
    total <- total - vec[i]*log2(vec[i])
  }
  total
}

countBlocks <- function(model){
  length(computeSModVec(cz(model)))
}

