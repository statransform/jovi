rm(list=ls())

cbPalette = c("#888888", "#E69F00", "#009E73", "#FF5E00")

w <- 2

curve(dlnorm(x-0.2, sd=0.5), xlim=c(0,4.5),col=cbPalette[1], lwd = w, frame.plot = FALSE, yaxt='n', ylab="", xlab="Time (sec)")
curve(dlnorm(x-0.5, sd=0.5), xlim=c(0,4.5),col=cbPalette[2], lwd = w, yaxt='n', ylab="", xlab="Time (sec)", add = TRUE)
curve(dlnorm(x-0.8, sd=0.5), xlim=c(0,4.5),col=cbPalette[3], lwd = w, yaxt='n', ylab="", xlab="Time (sec)", add = TRUE)
curve(dlnorm(x-1.1, sd=0.5), xlim=c(0,4.5),col=cbPalette[4], lwd = w, yaxt='n', ylab="", xlab="Time (sec)", add = TRUE)
