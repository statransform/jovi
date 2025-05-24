rm(list=ls())


df <- read.csv("p-values-noeffect-12.csv", sep=",", header=TRUE, strip.white=TRUE)

min = min(df$artX1,df$parX1)
max = max(df$artX1,df$parX1)

#min = min(df$pX1,df$artX1,df$parX1)
#max = max(df$pX1,df$artX1,df$parX1)

par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))

plot(df$parX1, df$artX1, ylim=c(min, max), xlim=c(min,max), col="blue", log='xy', frame.plot=F, ylab="ART p-value", xlab="PAR p-value", pch=4, main = "n = 12")
par(new=TRUE)
curve(1*x, add = T,col="grey")
curve(.05+0*x, add = T,col="red", lty =2)
text(min, .07, "alpha = .05", col = "red", pos = 4)
abline(v=0.05, col="red", lty =2)


df <- read.csv("p-values-noeffect-1000.csv", sep=",", header=TRUE, strip.white=TRUE)
min = min(df$pX1,df$artX1,df$intX1,df$parX1)
max = max(df$pX1,df$artX1,df$intX1,df$parX1)
plot(df$parX1,df$artX1, ylim=c(min, max), xlim=c(min,max), col="blue", log='xy', frame.plot=F, ylab="ART p-value", xlab="PAR p-value", pch=4, main = "n = 1000")
par(new=TRUE)
curve(1*x, add = T,col="grey")
curve(.05+0*x, add = T,col="red", lty =2)
text(min, .09, "alpha = .05", col = "red", pos = 4)
abline(v=0.05, col="red", lty =2)