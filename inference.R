source(jPaste(SBM_PATH, "visualize.R"))
source(jPaste(SBM_PATH, "mcmc.R"))
##library(igraph)

## read.csv(config-inference.csv)
## rather than using 'truth'
## in practice we won't know 'r', 'gamma', 'delta', etc.


jCat("ss = ", jPaste(config$searchStrategy))

sSearch <- eval(parse(text=jPaste(config$searchStrategy)))
jCat("a: sSearch = ")
print(sSearch)


load(file="truth") ##not needed!!

useRanking <- FALSE; useNetwork <- FALSE
objectiveRanking <- function(model) 0
objectiveNetwork <- function(model) 0

## the likelihood is determined by what data is available
if ("ranking" %in% dir()) {
    jCat("using edge ranking data")
  useRanking <- TRUE
  load(file="ranking")
  ## log P(pi | B)
  objectiveRanking <- function(model) loglikRankingVec(ranking,cz(model), truth$r) ##ToDo: remove "truth$"
}
if ("network" %in% dir()){
  jCat("using network data")
  useNetwork <- TRUE
  load(file="network")
  ## log P(A | B)
  objectiveNetwork <- function(model) loglikNetwork(network,cz(model), truth$gamma, truth$delta) ##ToDo: remove "truth$"
}

objective <- function(model) objectiveRanking(model) + objectiveNetwork(model)



if (!useRanking && !useNetwork){
  jCat("no data is available!")
}

prop <- NULL
##if(config$searchStrategy="sSearch_MH"){
  write(file="nIter.tex", config$nIter)
  write(file="nIterPerRestart.tex", config$nIterPerRestart)
  write(file="jumpSize.tex", config$jumpSize)
  prop <- function(x) gProposalFixedNLabels(x,config$jumpSize,numLabels)
##}

init(truth$sMod)  ##is this needed?


Labels <- truth$Labels ##(Labels <- LETTERS[1:numLabels])
##Q: is this needed? A: because we could conider more labels than the truth

(initial <- randomLabeling(length(truth$Nodes)))

Rprof("ram-profile.txt", memory.profiling=TRUE)
beforeT <- as.numeric(as.POSIXct(Sys.time()))

ssRun <- sSearch(prop, initial, objective, config$nIter, restart=config$nIterPerRestart, logNeglect=1000)

afterT <-  as.numeric(as.POSIXct(Sys.time()))
Rprof(NULL)

write(afterT - beforeT, file="time.txt")


##rename 'loglik' to 'loglikRanking'

##### P(B | pi, A)
##objective <- function(model) loglikRanking(ranking,model,truth$r, truth$gamma, truth$delta) + loglikNetwork(network)
##
##ssRun <- sSearch(prop, initial, objective, config$nIter, restart=config$nIterPerRestart, logNeglect=1000)



models <- keys(ssRun$samples)

jCat("ssRun = "); print(ssRun)
##jCat("ssRun$samples = ")
##print(ssRun$samples)

post <- sapply(models, function(x) exp(objective(x)))
jCat("post = ")
print(post)

save(ssRun, post, objective, file="ssRun")

