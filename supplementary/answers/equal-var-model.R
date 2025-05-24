rm(list=ls())


sdlog <- function(mean, sd) {
	sqrt(log(1 + sd^2/mean^2))	
}

meanlog <- function(mean, sd) {
	log(mean^2/sqrt(sd^2 + mean^2))
}

cbPalette = c("#888888", "#E69F00", "#009E73", "#FF5E00")

sd <- 0.8

curve(dlnorm(x-0.2, meanlog(1.2, sd), sdlog(1.2, sd)), xlim=c(0,4.5),col=cbPalette[1], lwd = 2, frame.plot = FALSE, yaxt='n', ylab="", xlab="Time (sec)")
curve(dlnorm(x-0.2, meanlog(1.5, sd), sdlog(1.5, sd)), xlim=c(0,4.5),col=cbPalette[2], lwd = 2, yaxt='n', ylab="", xlab="Time (sec)", add = TRUE)
curve(dlnorm(x-0.2, meanlog(1.8, sd), sdlog(1.8, sd)), xlim=c(0,4.5),col=cbPalette[3], lwd = 2, yaxt='n', ylab="", xlab="Time (sec)", add = TRUE)
curve(dlnorm(x-0.2, meanlog(2.1, sd), sdlog(2.1, sd)), xlim=c(0,4.5),col=cbPalette[4], lwd = 2, yaxt='n', ylab="", xlab="Time (sec)", add = TRUE)
