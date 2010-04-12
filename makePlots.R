load(file="scores")

randomColor <- function(alpha){
  r <- runif(1); g <- runif(1); b <- runif(1)
  total <- r+g+b
  rgb(r/total, g/total, b/total, alpha=alpha)
}

dotPlot <- function(scores, transparency=.2){
  plot(c(0,1), c(0,1), xlim=c(-0.2,1), ylim=c(0,nConsensa+1), type="n", yaxt="n", ylab="", xlab="Rand Index from the truth"); abline(v=c(0,1))
  
  pointLocs <- matrix(nrow=nConsensa, ncol=n)
  
  for (i in 1:nConsensa){
    abline(h=i, col="black")
    ##text(-0.1,i,t(results[1])[i])
    pointLocs[i,] <- rep(i,n)+0.4*(runif(n)-0.5)
    points(scores[,i],pointLocs[i,])
  }

  for (j in 1:n){
    colour <- randomColor(transparency)
    for(i in 2:nConsensa)      
      points(c(scores[j,i-1], scores[j,i]), c(pointLocs[i-1,j],pointLocs[i,j]), type="l", col=colour) ##col="#0000ff40")
  }
}

dotPlot(scores, transparency=0.5)
title("100 runs at r=3, different notions of consensus", xlab="Rand Index from the truth", col="white")

for (i in 1:nConsensa)
  text(0,i,consensa[i], pos=2)

##dev.print(pdf,"consensa(r=3).pdf")



countWorsts <- function(scores){
  nConsensa <- ncol(scores)
  worstCount <- rep(0,nConsensa)
  ##histogram of ranks
  ##for each row of scores, compute the sort permutation
  for (i in 1:n){
    consensa <- scores[i,]
    set <- which(consensa==min(consensa)) ##deal with multiple minima (ties for worst)
    for (j in seq_along(set)){
      worstCount[set[j]] <- worstCount[set[j]] + 1/length(set)
    }
  }
  worstCount
}
countWorsts(scores)

bestCount <- rep(0,nConsensa)
##histogram of ranks
##for each row of scores, compute the sort permutation
for (i in 1:n){
  consensa <- scores[i,]
  set <- which(consensa==max(consensa)) ##deal with multiple minima (ties for best)
  for (j in seq_along(set)){
    bestCount[set[j]] <- bestCount[set[j]] + 1/length(set)
  }
}

hist(worst, breaks=0.5:5.5)


scores2 <- scores[,-3]
countWorsts(scores2)

worst2 <- c()
for (i in 1:n){
  consensa <- scores2[i,]
  worst2[i] <- which.min(consensa)
}
hist(worst2, breaks=0.5:4.5)


nPerfect <- c()

##number of perfects
for (i in 1:nConsensa)
  nPerfect[i] <- sum(scores[,i]==1)



##for(i in 1:3){
##  points(scores[i,1:3], scores[i+1,3], type="l", col="grey")
##}

##for(i in 1:n){

##  for(j in 1:(nConsensa-1)){
##    points(scores[i,j], scores[i,j+1], type="l", col="grey")
##  }
##}

##dev.off()

