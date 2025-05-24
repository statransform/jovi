# Author: Theophanis Tsandilas, 2023-2024
# Inria & Université Paris-Saclay

# Experiment evaluating the Type I error rates of PAR, ART, RNK, and INT on ratio scales (distributions below) for a 4x3 within-subjects design
# See Fig. 9, Fig. 11, Fig. 12

# By default, this code will generate a data file for n = 20 with 5000 iteration. 
# You can edit the experimental parameters in the code to test different conditions.

rm(list=ls())

source("data-generators.R") # Import the data-generation functions

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

plotCellResponse <- function(df, x1s, x2 = "B1", min = 0, max = 8, legend = FALSE) {
	colors = c("#666666", "#E69F00", "#009E73", "#FF5E00")

	createText <- function(values){
		mean <- mean(values)
		sd <- sd(values)
		mlog <- log(mean^2/sqrt(sd^2 + mean^2)) # See: https://en.wikipedia.org/wiki/Log-normal_distribution
		sdlog <- sqrt(log(1 + sd^2/mean^2))	

		paste("mlog=", round(mlog,digits=2), ", sdlog=", round(sdlog, digits = 2), sep = "")
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


# Evaluate the following for main effects and interactions:
# A. Methods: (1) ANOVA/LMER with no transformation, (2) Rank, (3) ART, (4) INT.

# B. 4 x 3 repeated measures
design = c(4,3)

distr <- c("lnorm")
effects <- c(4,0,0)

# E. Cell sizes (n in the paper) -- for within-subject designs, it's also the number of subjects
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

