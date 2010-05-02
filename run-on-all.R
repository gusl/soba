## create 'nRuns' directories
source("~/.Rprofile")
(currentDir <- getwd())

config <- read.csv("config.csv", strip.white=TRUE)

args <- commandArgs(TRUE)
jCat("args = ", args)

## 3 levels: no plots, only plots, pdflatex

doPlots <- TRUE; doPdflatex <- TRUE
if (length(args)>0) {
  eval(parse(text=args[1]))
}


(folders <- dir(pattern="run[0-9]*"))
n <- length(folders)

for (i in 1:n){  

  dirName <- folders[i]
  jCat("\n\n************ Running on ", dirName)
  setwd(dirName)

  source(jPaste(SBM_PATH, "inference.R"))
  source(jPaste(SBM_PATH, "plotTopModels.R")) ##runs regardless of setting
  source(jPaste(SBM_PATH, "colabelingProbs.R"))
  source(jPaste(SBM_PATH, "consensus.R"))

  if(doPdflatex){
    t <- system(jPaste(PDFLATEX_PATH, " slides.tex"))
  }
  setwd("..")

}

## for (i in 1:3) alarm()
