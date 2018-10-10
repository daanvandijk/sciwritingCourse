library(robustbase)
library(MASS)

png(file='mcd.png', width=4, height=4, units="in", res=300)
group1 <- mvrnorm(n=1000, mu=c(0,0), Sigma=matrix(c(10,3,3,2),2))
group2 <- mvrnorm(n=500, mu=c(5,3), Sigma=matrix(c(5,4,4,10),2))
par(oma=c(0,0,0,0), mar=c(4,4,2,1))
plot(group1[,1], group1[,2], xlab='x', ylab='y')
points(group2[,1], group2[,2], xlab='x', ylab='y', col='red')
title('MCD method')
dev.off()

# The MLE
mu = c(0,0)
mu[1] <- mean(group1[,1])
mu[2] <- mean(group1[,2])
Sigma <- cov(group1)

# The MCD estimation
robust1 <- covMcd(group1)
robust2 <- covMcd(group1, alpha=0.75)

# Now write results of this simulation to a .tex file
handle <- file("mcd.tex")
writeLines("Hello world", handle)
close(handle)
