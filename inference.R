source(jPaste(SBM_PATH, "visualize.R"))
source(jPaste(SBM_PATH, "mcmc.R"))
library(igraph)

jCat("ss = ", jPaste(config$searchStrategy))

sSearch <- eval(parse(text=jPaste(config$searchStrategy)))
jCat("a: sSearch = ")
print(sSearch)


load(file="truth")
load(file="ranking")

write(file="nIter.tex", config$nIter)
write(file="nIterPerRestart.tex", config$nIterPerRestart)
write(file="jumpSize.tex", config$jumpSize)
init(truth$sMod)


Labels <- truth$Labels ##(Labels <- LETTERS[1:numLabels])
##Q: is this needed? A: because we could conider more labels than the truth

(initial <- randomLabeling(length(truth$Nodes)))

##### P(B | pi)
objective <- function(model) loglikVec(ranking,cz(model),truth$r)
prop <- function(x) gProposalFixedNLabels(x,config$jumpSize,numLabels)
##prop <- function(x) gProposal(x,config$jumpSize)

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

