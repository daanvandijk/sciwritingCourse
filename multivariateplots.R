library(robustbase)
library(MASS)

mu1 <- c(0,0)
Sigma1 <- matrix(c(10,3,3,2),2)
mu2 <- c(5,3)
Sigma2 <- matrix(c(5,4,4,10),2)
png(file='mcd.png', width=4, height=4, units="in", res=300)
group1 <- mvrnorm(n=1000, mu=mu1, Sigma=Sigma1)
group2 <- mvrnorm(n=500, mu=mu2, Sigma=Sigma2)
par(oma=c(0,0,0,0), mar=c(4,4,2,1))
plot(group1[,1], group1[,2], xlab='x', ylab='y')
points(group2[,1], group2[,2], xlab='x', ylab='y', col='red')
title('MCD method')
dev.off()

# Euclidean norm
norm <- function(x) sqrt(sum(x^2))

# Now write results of this simulation to a .tex file
handle <- file("mcd.tex")
lines <- c("\\begin{table}[h]",
           "\\centering",
           "\\begin{tabular}{l|l|l|l|l}",
           "\\% Outliers & Method & $|\\hat{\\mu} - \\mu|$ & $|\\hat{\\Sigma} - \\Sigma|$ & $\\log(\\text{Det.})$ \\\\ \\hline")
for (outliers in c(0,0.2,0.4)) {
  subgroup1 <- mvrnorm(n=1000 * (1-outliers), mu=mu1, Sigma=Sigma1)
  if (outliers > 0) {
    subgroup2 <- mvrnorm(n=1000 * outliers, mu=mu1, Sigma=Sigma2)
    group <- rbind(subgroup1, subgroup2)
  }
  else {
    group <- subgroup1
  }
  dim(group)
  # The MLE
  mle_mu = c(0,0)
  mle_mu[1] <- mean(group[,1])
  mle_mu[2] <- mean(group[,2])
  mle_Sigma <- cov(group)

  lines <- append(lines, sprintf("%d & MLE & %.2e & %.2e & - \\\\", outliers * 100, norm(mle_mu - mu1), norm(mle_Sigma - Sigma1)))
  
  # The MCD estimation
  robust1 <- covMcd(group)
  robust2 <- covMcd(group, alpha=0.75)
  
  lines <- append(lines, sprintf("%d & MCD (0.75) & %.2e & %.2e & %.2e \\\\", outliers * 100, norm(robust2$center - mu1), norm(robust2$cov - Sigma1), robust2$crit))
  lines <- append(lines, sprintf("%d & MCD (0.5) & %.2e & %.2e & %.2e \\\\", outliers * 100, norm(robust1$center - mu1), norm(robust1$cov - Sigma1), robust1$crit))
}
lines <- append(lines, c(
  "\\end{tabular}", 
  "\\caption{A comparison of ...}",
  "\\label{tab:mcd example}",
  "\\end{table}"
))

writeLines(lines, handle)
close(handle)
