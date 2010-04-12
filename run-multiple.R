##given a config.csv in the local directory
## create 'nRuns' directories
source("~/.Rprofile")
##(currentDir <- getwd())

##ToDo: option to not generate plots
config <- read.csv("config.csv")

doPlots <- TRUE; doPdflatex <- TRUE

for (i in 1:config$nRuns){
  save(i, file="runNumber")
  system("rm -rf current")
  system(jPaste("mkdir current"))
  system(jPaste("cp config.csv ../slides.tex current"))
  setwd("current")
  source(jPaste(SBM_PATH, "generate-true-structure.R"))
  source(jPaste(SBM_PATH, "simulate.R"))
  source(jPaste(SBM_PATH, "inference.R"))
  source(jPaste(SBM_PATH, "plotTopModels.R")) ##runs regardless of setting
  source(jPaste(SBM_PATH, "colabelingProbs.R"))
  source(jPaste(SBM_PATH, "consensus.R"))

  if(doPdflatex){
    t <- system(jPaste(PDFLATEX_PATH, " slides.tex"))
  }

  setwd("..")
  load(file="runNumber")

  nDigits <- log(config$nRuns, base=10)
  formattedNumber <- sprintf(jPaste("%0",nDigits,"d"), i)
  (command <- jPaste("mv current ", config$baseName, formattedNumber))
  system(command)
}
system("rm runNumber")
