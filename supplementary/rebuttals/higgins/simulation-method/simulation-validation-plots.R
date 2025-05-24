# Author: Theophanis Tsandilas, 2023-2024
# Inria & Université Paris-Saclay

# This code tests the simulation approach we used for the evaluation of the various rank methods (4x3 within-subjects). 
# The plots show:
# 1. The distributions at the latent space
# 2. The distribitions of the transformed values, representing the ditributions of the observed responses 

rm(list=ls())

source("data-generators.R") # Import the data-generation functions

# This is to plot the normal distributions
plotCellLatent <- function(df, x1s, x2 = "B1", min = -8, max = 8) {
	colors = c("#666666", "#E69F00", "#009E73", "#FF5E00")

	createText <- function(values){
		mean <- mean(values)
		sd <- sd(values)
		paste("m=", round(mean,digits=2), ", sd=", round(sd, digits = 2), sep = "")
	}

	values <- df[df$x1 == x1s[1] & df$x2 == x2, ]$dv
	plot(density(values), col = colors[1], lwd = 2, ylim = c(0, .4), xlim = c(min, max), main = paste("x2 =", x2), xlab = NA, bty="l")
	text(median(values) - 1, max(density(values)$y - .05), createText(values), pos = 2, col = colors[1])

	values <- df[df$x1 == x1s[2] & df$x2 == x2, ]$dv
	lines(density(values), col = colors[2], lwd = 2, ylim = c(0, .4), xlim = c(min, max))
	text(median(values)+.3, max(density(values)$y + .03), createText(values), pos = 2, col = colors[2])

	values <- df[df$x1 == x1s[3] & df$x2 == x2, ]$dv
	lines(density(values), col = colors[3], lwd = 2, ylim = c(0, .4), xlim = c(min, max))
	text(median(values)-.3, max(density(values)$y + .03), createText(values), pos = 4, col = colors[3])

	values <- df[df$x1 == x1s[4] & df$x2 == x2, ]$dv
	lines(density(values), col = colors[4], lwd = 2, ylim = c(0, .4), xlim = c(min, max))
	text(median(values)+1, max(density(values)$y -.05), createText(values), pos = 4, col = colors[4])
	
}

# This is to plot the transformed distributions
plotCellResponse <- function(df, x1s, x2 = "B1", min = 0, max = 8, legend = FALSE) {
	colors = c("#666666", "#E69F00", "#009E73", "#FF5E00")

	createText <- function(values){
		mean <- mean(values)
		sd <- sd(values)
		mlog <- log(mean^2/sqrt(sd^2 + mean^2)) # See: https://en.wikipedia.org/wiki/Log-normal_distribution
		sdlog <- sqrt(log(1 + sd^2/mean^2))	

		paste("mlog=", round(mlog,digits=2), ", sdlog=", round(sdlog, digits = 2), sep = "")
	}

	createTextAlt<- function(values){ # If you need to show the mean, sd of the responses use this instead
		mean <- mean(values)
		sd <- sd(values)
		paste("m=", round(mean,digits=2), ", sd=", round(sd, digits = 2), sep = "")
	}

	values <- df[df$x1 == x1s[1] & df$x2 == x2, ]$y
	plot(density(values), col = colors[1], lwd = 2, ylim = c(0, 1.65), xlim = c(min, max), main = paste("x2 =", x2), xlab = NA, bty="l")
	text(median(values), max(density(values)$y), createText(values), pos = 4, col = colors[1])
	if(legend) legend("topright", x1s, col=colors, lty=1, bty = "n", title = "x1")

	for(i in 2:4) {
		values <- df[df$x1 == x1s[i] & df$x2 == x2, ]$y
		lines(density(values), col = colors[i], lwd = 2, ylim = c(0, 1.65), xlim = c(min, max))
		text(median(values), max(density(values)$y), createText(values), pos = 4, col = colors[i])
	}
}


# 4 x 3 repeated measures
design = c(4,3)

distr <- c("lnorm") # Log-normal
effects <- c(4,0,0) # The effect parameters of the model: a1 = 4, a2 = 0, a12 = 0

# Sample size n
n <- 100000

# Set the seed for reproducibility
set.seed(1200)

df <- sim.within.2f(n, design, effects) %>% toDistribution(distr)
x1s = c("A1","A2","A3","A4")

par(mfrow = c(3, 2), mar = c(2, 4, 2, 1))

plotCellLatent(df, x1s, x2 = "B1")
plotCellResponse(df, x1s, x2 = "B1", legend = TRUE)

plotCellLatent(df, x1s, x2 = "B2")
plotCellResponse(df, x1s, x2 = "B2")

plotCellLatent(df, x1s, x2 = "B3")
plotCellResponse(df, x1s, x2 = "B3")

