source(jPaste(SBM_PATH, "visualize.R"))
source(jPaste(SBM_PATH, "mcmc.R"))
##library(igraph)

samplesHash <- ssRun$samples
mass <- ssRun$mass
nModels <- ssRun$nModels ##number of models visited

if (doPlots){
  pdf("slide-stochastic_search.pdf")
  plotStochasticSearchProgress(ssRun)
  dev.off()
}

sortedModels <- sort(values(ssRun$samples), decreasing=TRUE)

if (doPlots){
  pdf("slide-posterior_mass.pdf")
  plot(exp(sortedModels), type="h", xlab="rank", ylab="mass")
  dev.off()
}

if (isSimulationStudy){ 
  (truthIndex <- match(truth$structure, names(sortedModels)))
  if (!is.na(truthIndex)){ ##truth found
    write(file="truthIndex.tex", jPaste("The truth was the ",truthIndex,"th highest-scoring model found."))
  } else {
    postT <- sapply(sortedModels, function(x) exp(x))
    totalMass <- sum(postT)+exp(objective(cz(truth$structure)))
    proportionT <- exp(objective(cz(truth$structure)))/totalMass
    write(file="truthIndex.tex", jPaste("The truth was not found. If we add it to this set, it has ", signif(100*proportionT,3), "\\% of the mass."))
  }
}

########## SLIDE - inferred structures ##########
models <- keys(samplesHash) ##to be modified shortly (see 4 lines down)
post <- sapply(models, function(x) exp(objective(cz(x)))) ##ToDo: post <- values(samplesHash)
totalMass <- sum(post) ##(totalMass <- postMass(ssRun$samples,objective))
proportion <- sort(post, decreasing = TRUE)/totalMass
models <- names(proportion)

if(doPlots){   ##This one needs to get redone for the sake of the consensus computation - 26Mar2010
  pdf("slide-posterior.pdf", height=5)
}

if (isSimulationStudy){ 
  makeHeatmapPostRI(post,randIndex,12,makeText=TRUE, trueStructure=truth$structure, computeConsensus=TRUE)
} else {
  makeHeatmapPostRI(post,randIndex,12,makeText=TRUE, computeConsensus=TRUE)
}

if(doPlots){
  dev.off()
}
##}

nBlocks <- sapply(models,countBlocks)
entropies <- sapply(models,modelEntropy)


##ToDo: weighted average! Weight sd!

## now do the model averaging
write(file="entropies.tex", jPaste("mean = ", signif(weighted.mean(entropies,proportion),3)))
write(file="nBlocks.tex", jPaste("mean = ", signif(weighted.mean(nBlocks,proportion),3)))

if (isSimulationStudy){ 
  risFromTruth <- sapply(models, function(s) randIndex(cz(s),cz(truth$structure)))
  write(file="risFromTruth.tex", jPaste("mean = ", signif(weighted.mean(risFromTruth,proportion),3)))
}
## if distribution is bimodal, we would see some block structure (under some permutation)
