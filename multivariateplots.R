library(robustbase)
library(MASS)

experiment <- function(mu1, Sigma1, mu2, Sigma2, name, caption) {
  png(file=sprintf("%s.png", name), width=4, height=4, units="in", res=300)
  subgroup1 <- mvrnorm(n=1000, mu=mu1, Sigma=Sigma1)
  subgroup2 <- mvrnorm(n=500, mu=mu2, Sigma=Sigma2)
  group <- rbind(subgroup1, subgroup2)
  colors <- cbind(rep('black', 1000), rep('red', 500))
  par(oma=c(0,0,0,0), mar=c(4,4,2,1))
  plot(group[,1], group[,2], xlab='x', ylab='y', col=colors)
  title('MCD method')
  dev.off()

  # Euclidean norm
  norm <- function(x) sqrt(sum(x^2))

  # Now write results of this simulation to a .tex file
  handle <- file(sprintf("%s.tex", name))
  lines <- c("\\begin{table}[h]",
             "\\centering",
             "\\footnotesize",
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

  lines <- append(lines, c("\\end{tabular}", sprintf("\\caption{%s}", caption), "\\label{tab:mcd example}", "\\end{table}"))

  writeLines(lines, handle)
  close(handle)

  return(1)
}

caption1 = ""
caption2 = "A comparison of the MLE method, and the MCD (0.75) and MCD (0.5) methods where respectively 25\\% and 50\\% of the data is considered to be outliers.
  The data is distributed as $\\mathcal{N}(\\mu, \\Sigma)$, with $\\mu = (0,0)$ and $\\Sigma = \\begin{pmatrix}10 & 3\\ 3 & 2\\end{pmatrix}$. The outliers are distributed as $\\mathcal{N}(\\mu', \\Sigma')$ with $\\mu' = (5,3)$ and $\\Sigma' = \\begin{pmatrix}5 & 4\\ 4 & 10\\end{pmatrix}$.
  Notice that for $0\\%$ outliers the MLE behaves best, in the sense that its $\\Sigma$ estimator has the least error.
  Then for $20\\%$ outliers the MCD (0.75) behaves the best.
  And finally for $40\\%$ the MCD (0.5) is best."

experiment(c(0,0), matrix(c(10,3,3,2),2), c(10,10), matrix(c(4,1,1,4),2), "mcd-1", caption1)
experiment(c(0,0), matrix(c(10,3,3,2),2), c(5,3), matrix(c(5,4,4,10),2), "mcd-2", caption2)
