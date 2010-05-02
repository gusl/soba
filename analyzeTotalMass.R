source("~/.Rprofile")
setwd("c:/projects/sbm/R")
##not named "instances"



(folders <- select(function(x) file.info(x)$isdir & x!="instances", dir()))


j <- 0; res <- matrix(nrow=10, ncol=2)

for (f in folders){
  i <- 0
  j <- j+1
  jCat("dir(f) = ", dir(f))
  subfolders <- select(function(x) x!="config.csv", dir(f))
  jCat("subfolders = ", subfolders)
  jCat(length(subfolders))
  for (sf in subfolders){
    i <- i+1
    filePath <- jPaste(f,"/",sf,"/totalMass.tex")
    ##jCat("filePath = ", filePath)
    tm <- scan(filePath) ##'readLines' not needed
    jCat("i = ", i, "  j = ", j)
    res[i,j] <- tm
    ##jCat(tm)
  }
}

colnames(res) <- list("condition1", "condition2")

plot(log(res[,1],base=10), log(res[,2],base=10))

select <- function(test, l) l[test(l)] ##see 'subset'
select(function(x) !x %in% c("instances","config.csv"),dir())
