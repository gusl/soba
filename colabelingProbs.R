source(jPaste(SBM_PATH, "visualize.R"))
source(jPaste(SBM_PATH, "mcmc.R"))
library(igraph)

##load(file="ranking")

load(file="truth")

config <- read.csv("config.csv")
load(file="ssRun") ##loads 'post'

init(truth$sMod) ## shouldn't we replace this with a loaded variable?

samplesHash <- ssRun$samples

########## SLIDE - colabeling probabilities ##########
(colabelingProbs <- computeColabelingProbs(post))

if (doPlots){
  pdf("slide-colabelingprobs.pdf")
  makeHeatmapColabeling(colabelingProbs)
  dev.off()
}

solveQuadratic <- function(a,b,c)  (-b + sqrt(b^2- 4*a*c))/(2*a)

solveQuadratic(1/2,-1/2,-91)

## solving  (n^2 - n)/2 = 91   gives n = 14
convertToMatrix <- function(weights) {
  n <- solveQuadratic(1/2,-1/2,-length(weights))
  labels <- names(weights)
  mat <- matrix(rep(1,n^2),nrow=n, ncol=n)
  for (i in 1:(n-1)){
    for (j in (i+1):n){
      index <- match(jPaste(i,",",j), labels)      
      mat[i,j] <- weights[index]
      mat[j,i] <- weights[index]
    }
  }
  mat
}

##debug(convertToMatrix)
colPrMat <- convertToMatrix(colabelingProbs)

save(colPrMat,colabelingProbs,file="colabelingProbs")
