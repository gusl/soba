gustavo <- c(163.844, 417.391, 927.406)
gosset1 <- c(336.3746, 1048.065, 2262.547)
cypress <- c(103.65, 284.90, 582.25)

x <- c(9,18,27)

plot(x, gustavo, xlim=c(0,30), ylim=c(0,3000), type="n", main="blue: gosset1    black: Gustavo's machine   red: cypress", ylab="time in seconds (grey lines indicate multiples of 10 minutes)", xlab="problem size (number of genes)")
abline(h=c(0),v=0)
abline(h=600*(1:6), col="grey")
points(x,gosset1, col="blue", type="b")
points(x,gustavo, col="black", type="b")
points(x,cypress, col="red", type="b")
dev.print(pdf,"timing.pdf")
