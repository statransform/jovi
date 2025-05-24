rm(list=ls())


df <- read.csv("p-values-noeffect-12.csv", sep=",", header=TRUE, strip.white=TRUE)

min = min(df$pX1,df$artX1,df$intX1,df$parX1)
max = max(df$pX1,df$artX1,df$intX1,df$parX1)

#min = min(df$pX1,df$artX1,df$parX1)
#max = max(df$pX1,df$artX1,df$parX1)

par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))

plot(df$pX1,df$artX1, ylim=c(min, max), xlim=c(min,max), col="blue", log='xy', frame.plot=F, ylab="p-value", xlab="reference p-value (log)", pch=4, main = "n = 12")
par(new=TRUE)
plot(df$pX1,df$parX1, ylim=c(min, max), xlim=c(min,max),  col = rgb(0.1, .7, 0.1, 0.8), log='xy', bty ="n", axes=F, frame.plot=F, xaxt='n', yaxt='n', xlab=NA, ylab=NA, pch=5)
par(new=TRUE)
#plot(df$pX1,df$intX1,  ylim=c(min, max), xlim=c(min,max), log='xy', col = "blue", bty ="n", axes=F, frame.plot=F, xaxt='n', yaxt='n', xlab=NA, ylab=NA)
curve(1*x, add = T,col="grey")
curve(.05+0*x, add = T,col="red")
text(min, .07, "alpha = .05", col = "red", pos = 4)
curve(.001+0*x, add = T,col="red", lty =2)
text(min, .0015, "alpha = .001", col = "red", pos = 4)

#text(.0006, 0.5, "PAR", col = rgb(0.1, .7, 0.1, 0.8), pos = 4)
#text(.0006, 0.0001, "ART", col = "blue", pos = 4)


df <- read.csv("p-values-noeffect-1000.csv", sep=",", header=TRUE, strip.white=TRUE)
min = min(df$pX1,df$artX1,df$intX1,df$parX1)
max = max(df$pX1,df$artX1,df$intX1,df$parX1)
plot(df$pX1,df$artX1, ylim=c(min, max), xlim=c(min,max), col="blue", log='xy', frame.plot=F, ylab="p-value", xlab="reference p-value (log)", pch=4, main = "n = 1000")
par(new=TRUE)
plot(df$pX1,df$parX1, ylim=c(min, max), xlim=c(min,max), col = rgb(0.1, .7, 0.1, 0.8), log='xy', bty ="n", axes=F, frame.plot=F, xaxt='n', yaxt='n', xlab=NA, ylab=NA, pch=5)
par(new=TRUE)
#plot(df$pX1,df$intX1,  ylim=c(min, max), xlim=c(min,max), log='xy', col = "blue", bty ="n", axes=F, frame.plot=F, xaxt='n', yaxt='n', xlab=NA, ylab=NA)
curve(1*x, add = T,col="grey")
curve(.05+0*x, add = T,col="red")
text(min, .09, "alpha = .05", col = "red", pos = 4)
curve(.001+0*x, add = T,col="red", lty =2)
text(min, .002, "alpha = .001", col = "red", pos = 4)

