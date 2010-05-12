source(jPaste(SBM_PATH, "visualize.R"))
source(jPaste(SBM_PATH, "mcmc.R"))
##library(igraph)

load(file="truth")

init(truth$sMod)

if(config$generateRanking) {
  (run <- generateRankings(truth$r,1))
  (ranking <- run$arrivals[[1]])
  ## (ranking <- generateRanking(truth$r))

  if (doPlots){
    pdf("slide-ranking.pdf",height=2.5,width=7)
    plotRankings(run,title="",makeText=FALSE,makeBalls=TRUE)
    dev.off()
                                       ##}  if (doPlots){
    pdf("slide-ranking2.pdf")
    makeHeatmapRanking(ranking)
    dev.off()
  }
  save(ranking, file="ranking")
}


if(config$generateNetwork) {
  (network <- generateNetwork(truth$gamma,truth$delta))

  if (doPlots){
    pdf("slide-network.pdf",height=2.5,width=7)
    visualizeStructure(network,title="simulated network")
    dev.off()
  }
  save(network, file="network")
}












