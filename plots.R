x = c(1, 2, 3, 4, 5)
y = c(1.1, 1.9, 3.2, 4.0, 5.3)

# todo: no colors; dashline and dashcircles?
 
png(file='without.png', width=4, height=4, units="in", res=300)
par(oma=c(0,0,0,0), mar=c(4,4,2,1))
plot(x, y, xlab='x', ylab='y')
abline(lm(x~y))
title('LS without outlier')
dev.off()

png(file='with.png', width=4, height=4, units="in", res=300)
par(oma=c(0,0,0,0), mar=c(4,4,2,1))
x2 = c(1, 4.5, 3, 4, 5)
y2 = c(1.1, 1.9, 3.2, 4.0, 5.3)
plot(x2, y2, xlab='x', ylab='y', col=ifelse(x==2, "red", "black"))
abline(lm(x~y), col='blue')
abline(lm(x2~y2), col='red')
legend(1, 5, c("without", "with"), lty=1, col=c("blue", "red"))
title('LS with outlier')
dev.off()