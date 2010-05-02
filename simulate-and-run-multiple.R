##given a config.csv in the local directory
## create 'nRuns' directories
source("~/.Rprofile")
##(currentDir <- getwd())


doPlots <- TRUE; doPdflatex <- TRUE

args <- commandArgs(TRUE)
if (length(args)>0) eval(parse(text=args[1]))



##ToDo: option to not generate plots
config <- read.csv("config.csv", strip.white=TRUE)

jCat("RUN-MULTIPLE.R")
jCat("truth = ", config$truth)
jCat("rtrue = ", config$rtrue)
jCat("gamma = ", config$gamma)
jCat("delta = ", config$delta)
jCat("searchStrategy = ", jPaste(config$searchStrategy))

jCat("nRuns = ", config$nRuns)


##config$searchStrategy <- sub(" ", "", config$searchStrategy)
config$baseName <- sub(" ", "", config$baseName)
jPaste("config$baseName = ", config$baseName)

                       
for (runIndex in 1:config$nRuns){

  nDigits <- floor(log(config$nRuns, base=10))+1
  jCat(nDigits)
  nDigitsI <- floor(log(runIndex, base=10))+1
  jCat(nDigitsI)
  
  jCat("(a)  runIndex = ", runIndex)

  formattedNumber <- jPaste(concat(rep("0",nDigits - nDigitsI)),runIndex)  ##sprintf(jPaste("%0",nDigits,"d"), i)
  folderName <- sub(" ", "", jPaste(config$baseName,formattedNumber))
  jCat("folderName = ", folderName)
  
  
  ##system("rm -rf current")
  system(jPaste("mkdir ", folderName))
  system(jPaste("cp config.csv ../slides.tex ", folderName))
  jCat("going there")
  print(getwd())
  setwd(folderName)
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
  jCat("coming back")
  print(getwd())
  jCat("(b)  runIndex = ", runIndex)
  
}

