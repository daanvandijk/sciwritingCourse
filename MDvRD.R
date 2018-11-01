library(car)
library(MASS)
library(rrcov)

data <- mvrnorm(n=10, mu=c(0,0), Sigma=matrix(c(2.1,2,2,2),2))
outliers <- mvrnorm(n=5, mu=c(0,2), Sigma=matrix(c(3,1,1,3),2))
exampleset <- rbind(data,outliers)
colors = cbind(rep('black', 10), rep('red', 5))

png(file='scatterplot.png', width=4, height=4, units="in", res=300)
par(oma=c(0,0,0,0), mar=c(4,4,2,1))
plot(exampleset, xlab = 'x1', ylab = 'x2', col=colors)
text(exampleset[11:15,], labels=c(11,12,13,14,15), pos = 4)
title('Scatterplot')
dev.off()

mu <- colMeans(exampleset)
Sigma <- cov(exampleset)
D2 <- mahalanobis(exampleset, mu, Sigma)
sqrt(D2)

MD <- cbind(c(1:15), sqrt(D2))

png(file='classical-estimators.png', width=4, height=4, units="in", res=300)
par(oma=c(0,0,0,0), mar=c(4,4,2,1))
plot(MD, xlab='Observation', ylab='MD', col=colors)
abline(h=sqrt(5.991), col='blue')
text(MD[11:15,], labels=c(11,12,13,14,15), pos = 2)
title('Classical estimators')
dev.off()

test <- cov.mcd(exampleset)
muMCD <- test$center
SigmaMCD <-test$cov
D2MCD <- mahalanobis(exampleset, muMCD, SigmaMCD)
sqrt(D2MCD)
RD <- cbind(c(1:15), sqrt(D2MCD))

png(file='mcd-estimators.png', width=4, height=4, units="in", res=300)
par(oma=c(0,0,0,0), mar=c(4,4,2,1))
plot(RD, xlab='Observation', ylab='MD', col=colors)
text(RD[11:15,], labels=c(11,12,13,14,15), pos = 2)
abline(h=sqrt(5.991), col='blue')
title('MCD estimators')
dev.off()
