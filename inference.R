source("~/.Rprofile")
source(jPaste(SBM_PATH, "visualize.R"))
source(jPaste(SBM_PATH, "mcmc.R"))
library(igraph)

load(file="truth")
load(file="ranking")

config <- read.csv("config.csv")
write(file="nIter.tex", config$nIter)
write(file="nIterPerRestart.tex", config$nIterPerRestart)
write(file="jumpSize.tex", config$jumpSize)

init(truth$sMod)

Labels <- truth$Labels ##(Labels <- LETTERS[1:numLabels])
##Q: is this needed? A: because we could conider more labels than the truth

(initial <- randomLabeling(length(truth$Nodes)))
objective <- function(model) loglikVec(ranking,model,truth$r)
prop <- function(x) gProposalFixedNLabels(x,config$jumpSize,numLabels)
##prop <- function(x) gProposal(x,config$jumpSize)


Rprof("ram-profile.txt", memory.profiling=TRUE)
beforeT <- as.numeric(as.POSIXct(Sys.time()))
ssRun <- sSearch(prop, initial, objective, config$nIter, restart=config$nIterPerRestart, logNeglect=1000)
afterT <-  as.numeric(as.POSIXct(Sys.time()))
Rprof(NULL)


write(afterT - beforeT, file="time.txt")



##load(file="ssRun")

models <- keys(ssRun$samples)
post <- sapply(models, function(x) exp(objective(cz(x))))

save(ssRun, post, objective, file="ssRun")

