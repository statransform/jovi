rm(list=ls())


df <- read.csv("p-values-12-interaction.csv", sep=",", header=TRUE, strip.white=TRUE)

min = min(df$pX1X2,df$artX1X2,df$intX1X2,df$parX1X2)
max = max(df$pX1X2,df$artX1X2,df$intX1X2,df$parX1X2)

#min = min(df$pX1,df$artX1,df$parX1)
#max = max(df$pX1,df$artX1,df$parX1)

par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))

plot(df$pX1X2,df$artX1X2, ylim=c(min, max), xlim=c(min,max), col="blue", log='xy', frame.plot=F, ylab="p-value", xlab="reference p-value (log)", pch=4, main = "n = 12")
par(new=TRUE)
plot(df$pX1X2,df$parX1X2, ylim=c(min, max), xlim=c(min,max),  col = rgb(.6,.6,.6, 0.8), log='xy', bty ="n", axes=F, frame.plot=F, xaxt='n', yaxt='n', xlab=NA, ylab=NA, pch=5)
par(new=TRUE)
#plot(df$pX1,df$intX1,  ylim=c(min, max), xlim=c(min,max), log='xy', col = "blue", bty ="n", axes=F, frame.plot=F, xaxt='n', yaxt='n', xlab=NA, ylab=NA)
curve(1*x, add = T,col="grey")

text(.0000001, max, "PAR", col = rgb(.6,.6,.6), pos = 4)
text(.0000001, 0.2, "ART", col = "blue", pos = 4)


#df <- read.csv("p-values-noeffe.csv", sep=",", header=TRUE, strip.white=TRUE)
min = min(df$pX1X2, df$rnkX1X2,df$intX1X2, df$artX1X2)
max = max(df$pX1X2, df$rnkX1X2,df$intX1X2, df$artX1X2)

#min = min(df$pX1X2, df$rnkX1X2,df$intX1X2)
#max = max(df$pX1X2, df$rnkX1X2,df$intX1X2)

plot(df$pX1X2,df$rnkX1X2, ylim=c(min, max), xlim=c(min,max), col="orange", log='xy', frame.plot=F, ylab="p-value", xlab="reference p-value (log)", pch=4, main = "n = 12")
par(new=TRUE)
plot(df$pX1X2,df$intX1X2, ylim=c(min, max), xlim=c(min,max), col = rgb(0.1, .7, 0.1, 0.8), log='xy', bty ="n", axes=F, frame.plot=F, xaxt='n', yaxt='n', xlab=NA, ylab=NA, pch=5)
par(new=TRUE)
#plot(df$pX1,df$intX1,  ylim=c(min, max), xlim=c(min,max), log='xy', col = "blue", bty ="n", axes=F, frame.plot=F, xaxt='n', yaxt='n', xlab=NA, ylab=NA)
curve(1*x, add = T,col="grey")

text(.0000001, max, "RNK", col = "orange", pos = 4)
text(.0000001, 0.2, "INT", col = rgb(0.1, .7, 0.1, 0.8), pos = 4)