source(jPaste(SBM_PATH, "visualize.R"))
source(jPaste(SBM_PATH, "mcmc.R"))
library(igraph)

load(file="truth")

init(truth$sMod)

(run <- doExpt(truth$r,1))

if (doPlots){
  pdf("slide-ranking.pdf",height=2.5,width=7)
  plotRuns(run,title="",makeText=FALSE,makeBalls=TRUE)
  dev.off()
}

(ranking <- run$arrivals[[1]])
if (doPlots){
  pdf("slide-ranking2.pdf")
  makeHeatmapRanking(ranking)
  dev.off()
}

save(ranking, file="ranking")
