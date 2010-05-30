## create 'nRuns' directories
source("~/.Rprofile")
source(jPaste(SBM_PATH,"conversion.R"))

(currentDir <- getwd())
args <- commandArgs(TRUE)
jCat("args = ", args)
## 3 levels: no plots, only plots, pdflatex

doPlots <- TRUE; doPdflatex <- TRUE
if (length(args)>0) {
  eval(parse(text=args[1]))
}


config <- read.csv("config.csv", strip.white=TRUE)
isSimulationStudy <- !is.na(config$truth) ##i.e. a truth is given
inspect(isSimulationStudy)


(folders <- dir(pattern="run[0-9]*"))
n <- length(folders)

for (i in 1:n){  

  dirName <- folders[i]
  jCat("\n\n************ Running on ", dirName)
  setwd(dirName)

  ##sink(file="log.txt")
    
  files <- dir()
  if("EdgeRankingData.txt" %in% files) initFromRealData(config$k)
  if("EdgeRankingData.txt" %in% files) convertEdgeRankingDataFile() ##saves a file named "ranking"
  if("NetworkData.txt" %in% files) convertNetworkDataFile() ##saves a file named "network"
  
  source(jPaste(SBM_PATH, "inference.R"))  ##'inference' is blind to what kind of study this is; reads "ranking" and "network"
  jCat("@2")
  source(jPaste(SBM_PATH, "plotTopModels.R")) ##runs regardless of setting
  jCat("@3")
  source(jPaste(SBM_PATH, "colabelingProbs.R"))
  jCat("@4")
  source(jPaste(SBM_PATH, "consensus.R"))
  jCat("@5")
  ##sink()
  
  if(doPdflatex){
    t <- system(jPaste(PDFLATEX_PATH, " slides.tex"))
  }
  setwd("..")

}

## for (i in 1:3) alarm()
