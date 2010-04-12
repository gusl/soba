### This file is to be run remotely
                                        # all sub-directories
(folders <- dir(pattern="run[0-9]*"))
(n <- length(folders))

consensa <- c("randDist", "colPrDist", "cut", "thresh", "mode") ## ToDo: get this from consensusResults.csv
nConsensa <- length(consensa)

scores <- matrix(ncol=nConsensa,nrow=n)
count <- 0
for (f in folders){
  count <- count+1
  (filePath <- jPaste(f,"/consensusResults.csv"))
  results <- read.csv(filePath)
  scoresHere <- t(results[3])
  scores[count,] <- scoresHere
}

print(list(n=n, consensa=consensa, nConsensa=nConsensa, folders=folders, scores=scores))
save(n, nConsensa, folders, scores, file="scores")

