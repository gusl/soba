## create 'nRuns' directories
source("~/.Rprofile")
(currentDir <- getwd())

args <- commandArgs(TRUE)
jCat("args = ", args)

## 3 levels: no plots, only plots, pdflatex

doPlots <- TRUE; doPdflatex <- TRUE
if (length(args)>0) {
  eval(parse(text=args[1]))
}
##  Rscript ../fix-multiple.R "doPlots=FALSE; doPdflatex=FALSE"

##ToDo: option to not generate plots
##config <- read.csv("config.csv")

(folders <- dir(pattern="run[0-9]*"))
n <- length(folders)

for (i in 1:n){  
  save(i, file="runNumber")
  dirName <- folders[i]
  jCat("dirName = ", dirName)
  setwd(dirName)
  system("rm consensusResults.csv")
  load(file="ssRun")
  ##jCat("-------------1")
  source(jPaste(SBM_PATH, "plotTopModels.R")) ##runs regardless of setting
  ##jCat("-------------2")
  source(jPaste(SBM_PATH, "colabelingProbs.R"))
  ##jCat("-------------3")
  source(jPaste(SBM_PATH, "consensus.R"))
  ##jCat("-------------4")
  if(doPdflatex){
    t <- system(jPaste(PDFLATEX_PATH, " slides.tex"))
  }
  setwd("..")
  load(file="runNumber")
}
system("rm runNumber")
