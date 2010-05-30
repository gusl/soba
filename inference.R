source(jPaste(SBM_PATH, "visualize.R"))
source(jPaste(SBM_PATH, "mcmc.R"))
##library(igraph)

## read.csv(config-inference.csv)
## rather than using 'truth'
## in practice we won't know 'r', 'gamma', 'delta', etc.


jCat("ss = ", jPaste(config$searchStrategy))


parseString <- function(s)  strsplit(s,"[\\(\\),]")[[1]]

jCat("config$searchStrategy = ", jPaste(config$searchStrategy))
parsedStrategy <- parseString(jPaste(config$searchStrategy))
jCat("parsedStrategy = ", parsedStrategy)
searchStrategy <- parsedStrategy[1] ##just use the base name

sSearch <- eval(parse(text=jPaste(searchStrategy)))
##inspect(sSearch)
jCat("a: sSearch = ")
print(sSearch)


##load(file="truth") ##not needed!!

useRanking <- FALSE; useNetwork <- FALSE
objectiveRanking <- function(model) 0
objectiveNetwork <- function(model) 0

## the likelihood is determined by what data is available
if ("ranking" %in% dir()) {
  jCat("using edge ranking data")
  useRanking <- TRUE
  load(file="ranking")
  ## log P(pi | B)
  objectiveRanking <- function(model) loglikRankingVec(ranking,cz(model), config$r) ##ToDo: remove "truth$"
}
if ("network" %in% dir()){
  jCat("using network data")
  loglikNetwork <- eval(parse(text=jPaste("loglikNetwork_",config$missingNetworkDataModel)))  
  useNetwork <- TRUE    
  load(file="network")
    
  ## log P(A | B)
  objectiveNetwork <- function(model) loglikNetwork(network, cz(model), config$gamma, config$delta) ##ToDo: remove "truth$"
}

objective <- function(model) objectiveRanking(model) + objectiveNetwork(model)



if (!useRanking && !useNetwork){
  jCat("no data is available!")
}

prop <- NULL
if(config$searchStrategy=="sSearch_MH"){
  write(file="nIter.tex", config$nIter)
  write(file="nIterPerRestart.tex", config$nIterPerRestart)
  write(file="jumpSize.tex", config$jumpSize)
}

cprime <- NULL
if(searchStrategy=="sSearch_MOSS"){
  jCat("The search strategy is MOSS.")
}

Rprof("ram-profile.txt", memory.profiling=TRUE)
beforeT <- as.numeric(as.POSIXct(Sys.time()))

ssRun <- sSearch(objective, parsedStrategy)

afterT <-  as.numeric(as.POSIXct(Sys.time()))
Rprof(NULL)

write(afterT - beforeT, file="time.txt")



models <- keys(ssRun$samples)

jCat("ssRun = "); print(ssRun)

post <- sapply(models, function(x) exp(objective(x)))
jCat("post = ")
print(post)

save(ssRun, post, objective, file="ssRun")

