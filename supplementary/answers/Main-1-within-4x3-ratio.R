# Author: Theophanis Tsandilas, 2023-2024
# Inria & Université Paris-Saclay

# Experiment evaluating the Type I error rates of PAR, ART, RNK, and INT on ratio scales (distributions below) for a 4x3 within-subjects design
# See Fig. 9, Fig. 11, Fig. 12

# By default, this code will generate a data file for n = 20 with 5000 iteration. 
# You can edit the experimental parameters in the code to test different conditions.

rm(list=ls())
library(lmerTest)

# The ARTool
library(ARTool)

# Parallel computation
library(foreach)
library(doParallel)

source("data-generators.R") # Import the data-generation functions

getLmerPValues <- function(model, vars) {
	return(anova(model)[vars, 6])
}


getARTPValues <- function(model, vars) {
	return(anova(model)[vars, 5]) # For within-subjects
}

# Repeated measures with two factors
test.within.2f <- function(n, design, effects, distr){ 
	df <- sim.within.2f(n, design, effects) %>% toDistribution(distr)

	model0 <- suppressMessages(lmer(y ~ x1*x2 + (1|s), data=df)) # Parametric
	model1 <- suppressMessages(lmer(rank(y) ~ x1*x2 + (1|s), data=df)) # RNK
	model2 <- suppressMessages(art(y ~ x1*x2 + (1|s), data=df)) # ARTool
	model3 <- suppressMessages(lmer(inverseNormalTransform(y) ~ x1*x2 + (1|s), data=df)) # INT
	vars <- c("x1", "x2", "x1:x2")
	c(getLmerPValues(model0, vars), getLmerPValues(model1, vars), getARTPValues(model2, vars), getLmerPValues(model3, vars))
}

# Performs repetitive tests for the given configuration
test <- function(repetitions, n, design, effects, distr) {
	results <- foreach(rid = 1:repetitions, .combine=rbind) %dopar% {
		tryCatch(
			{
				test.within.2f(n, design, effects, distr)
			}, 
			error = function(cond) {
				# do nothing
			}, finally = {
				# do nothing
			}
		)
	}

	# From p-values to positive rates that represent (depending on whether there is a true effect) either Type I error rates (false positives) or power (true positives) 
	res.05 <- round(colMeans(results<.05, na.rm = TRUE), digits = 4) # alpha = .05
	res.01 <- round(colMeans(results<.01, na.rm = TRUE), digits = 4) # alpha = .01

	designStr <- paste(design, collapse="x")

	# Split the results into separate rows 
	return(tribble(~n, ~design, ~distr, ~method, ~alpha, ~effects, ~rates,
			n, designStr, distr, "PAR", 0.05, effects, res.05[1:3],		
			n, designStr, distr, "RNK", 0.05, effects, res.05[4:6],
			n, designStr, distr, "ART", 0.05, effects, res.05[7:9],
			n, designStr, distr, "INT", 0.05, effects, res.05[10:12],

			n, designStr, distr, "PAR", 0.01, effects, res.01[1:3],		
			n, designStr, distr, "RNK", 0.01, effects, res.01[4:6],
			n, designStr, distr, "ART", 0.01, effects, res.01[7:9],
			n, designStr, distr, "INT", 0.01, effects, res.01[10:12]
		)
	)
}

# Evaluate the following for main effects and interactions:
# A. Methods: (1) ANOVA/LMER with no transformation, (2) Rank, (3) ART, (4) INT.

# B. 4 x 3 repeated measures
design = c(4,3)
distr <- "lnorm"

effect <- c(4,0,0)
n <- 1000

df <- sim.within.2f(n, design, effect) %>% toDistribution(distr)
model0 <- suppressMessages(lmer(y ~ x1*x2 + (1|s), data=df)) # Parametric
model1 <- suppressMessages(lmer(rank(y) ~ x1*x2 + (1|s), data=df)) # RNK
model2 <- suppressMessages(art(y ~ x1*x2 + (1|s), data=df)) # ARTool
model3 <- suppressMessages(lmer(inverseNormalTransform(y) ~ x1*x2 + (1|s), data=df)) # INT
model4 <- suppressMessages(lmer(log(y) ~ x1*x2 + (1|s), data=df)) # LOG
vars <- c("x1", "x2", "x1:x2")
res <- c(getLmerPValues(model0, vars), getLmerPValues(model2, vars), getARTPValues(model1, vars), getLmerPValues(model3, vars), getLmerPValues(model4, vars))



# Store the results
#csvfile <- paste("logs/", filename, format(Sys.time(), "_%s"), ".csv", sep="")
#write.csv(res, file = csvfile, row.names=FALSE, quote=F)

