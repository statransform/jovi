# Author: Theophanis Tsandilas, 2023-2024
# Inria & Université Paris-Saclay

# Experiment evaluating the Type I error rates of PAR, ART, RNK, and INT for a 2x3 between-subjects to investigate Type I error rate on a different factor
# See Fig. 18

# It may take several hours to complete. 
# You can edit the experimental parameters in the code to test different conditions.

rm(list=ls())
library(lmerTest)

# The ARTool
library(ARTool)

# Parallel computation
library(foreach)
library(doParallel)

source("data-generators.R") # Import the data-generation functions

getLmerPValues <- function(model, vars, lmer = TRUE) {
	return(suppressMessages(anova(model)[vars, ifelse(lmer, 6, 5)])) # For aov models the table is different
}

getARTPValues <- function(model, vars, lmer = TRUE) {
	return(suppressMessages(anova(model)[vars, ifelse(lmer, 5, 7)])) # For aov models the ART's table is different
}

# Between subjects with two factors
test.between.2f <- function(n, design, effects, distr){ 
	df <- sim.between.2f(design[1]*design[2]*n, design, effects) %>% toDistribution(distr)  # Ensure n participants per cell

	model0 <- suppressMessages(aov(y ~ x1*x2, data=df)) # Parametric
	model1 <- suppressMessages(aov(rank(y) ~ x1*x2, data=df)) # RNK
	model2 <- suppressMessages(art(y ~ x1*x2, data=df)) # ARTool
	model3 <- suppressMessages(aov(inverseNormalTransform(y) ~ x1*x2, data=df)) # INT

	empty <- c(1, 1, 1, 1) #empty columns to add to the results for "x3", "x1:x3", "x2:x3", "x1:x2:x3" (for consistency with the 3-factor design)
	vars <- c("x1", "x2", "x1:x2")
	c(getLmerPValues(model0, vars, FALSE), empty, getLmerPValues(model1, vars, FALSE), empty, getARTPValues(model2, vars, FALSE), empty, getLmerPValues(model3, vars, FALSE), empty)
}


# Mixed with one between-participants factor and one repeated-measures factor
test.mixed.2f <- function(n, design, effects, distr){
	df <- sim.mixed.2f(design[1]*n, design, effects) %>% toDistribution(distr) # Ensure n participants per cell
	
	model0 <- suppressMessages(lmer(y ~ x1*x2 + (1|s), data=df)) # Parametric
	model1 <- suppressMessages(lmer(rank(y) ~ x1*x2 + (1|s), data=df)) # RNK
	model2 <- suppressMessages(art(y ~ x1*x2 + (1|s), data=df)) # ARTool
	model3 <- suppressMessages(lmer(inverseNormalTransform(y) ~ x1*x2 + (1|s), data=df)) # INT

	empty <- c(1, 1, 1, 1) #empty columns to add to the results for "x3", "x1:x3", "x2:x3", "x1:x2:x3" (for consistency with the 3-factor design)
	vars <- c("x1", "x2", "x1:x2")	
	c(getLmerPValues(model0, vars), empty, getLmerPValues(model1, vars), empty, getARTPValues(model2, vars), empty, getLmerPValues(model3, vars), empty)
}

testFun <- function(id) {switch(id,test.between.2f, test.mixed.2f)} 

# Performs repetitive tests for the given configuration
test <- function(repetitions, n, designId, design, effects, distr) {
	results <- foreach(rid = 1:repetitions, .combine=rbind) %dopar% {
		tryCatch(
			{
				testFun(designId)(n, design, effects, distr)
			}, 
			error = function(cond) {
				# do nothing
			}, finally = {
				# do nothing
			}
		)
	}

	# From p-values to positive rates that represent (depending on whether there is a true effect) either Type I error rates (false positives) or power (true positives) 
	res.05 <- round(colSums(results<0.05)/nrow(results), digits = 4) # alpha = .05
	res.01 <- round(colSums(results<0.01)/nrow(results), digits = 4) # alpha = .01

	designStr <- paste(design, collapse="x")

	# Split the results into separate rows 
	return(tribble(~n, ~design, ~distr, ~method, ~alpha, ~effects, ~rates,
			n, designStr, distr, "PAR", 0.05, effects, res.05[1:7],
			n, designStr, distr, "RNK", 0.05, effects, res.05[8:14],
			n, designStr, distr, "ART", 0.05, effects, res.05[15:21],
			n, designStr, distr, "INT", 0.05, effects, res.05[22:28],

			n, designStr, distr, "PAR", 0.01, effects, res.01[1:7],		
			n, designStr, distr, "RNK", 0.01, effects, res.01[8:14],
			n, designStr, distr, "ART", 0.01, effects, res.01[15:21],
			n, designStr, distr, "INT", 0.01, effects, res.01[22:28]
		)
	)
}

# Evaluate the following for main effects and interactions:
# A. Methods: (1) ANOVA/LMER with no transformation, (2) Rank, (3) ART, (4) INT.

# B. (1) 2 x 3 between-subjects
designs <- list(c(2,3))

# C. Continuous distributions (equal variance, full), discrete distribution: binom (size = 10, prob = 0.1) and Poisson, ordinal with 5 levels (fexible threshlods)
distributions <- c("norm", "lnorm", "exp", "poisson", "binom", "likert5B")

# D. Various combinations of effects
effects <- matrix(c(0, 0, 0, 0,
				    0, 0.5, 0, 0,
				    0, 1, 0, 0,
				    0, 2, 0, 0,
				    0, 4, 0, 0,
				    0, 8, 0, 0), 
				   ncol = 4, byrow = TRUE)

# E. Cell sizes (n in the paper)
Ns <- c(10, 20, 30) 
#Ns <- c(20) # We only test n = 20

# 5000 iterations
R <- 5000

filename = "3_test-Designs_2x3"

# Set the seed for reproducibility
set.seed(4440)

#Parallel: https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html
CoresNum <- 4
registerDoParallel(CoresNum)  # use multicore, set to the number of our cores

time <- system.time({ 
	results <- foreach(desId = 1:length(designs), .combine=rbind) %do% {
		foreach(distr = distributions, .combine=rbind) %do% {
			foreach(n = Ns, .combine=rbind) %do% {
				foreach(effId = 1:nrow(effects), .combine=rbind) %do% {
					# The test function is parallelized (using multiple cores)
					test(repetitions = R, n = n, designId = desId, design = designs[[desId]], effects = effects[effId,], distr = distr)
				}
			}
		}
	}
})

# split the effects and rates columns (currently vectors) into individual columns
res <- results %>% unnest_wider(effects, names_sep = "_") %>% unnest_wider(rates, names_sep = "_")

colnames(res)[6:9]=c("effectX1","effectX2","effectX1X2","effectX3")
colnames(res)[10:16]=c("rateX1","rateX2","rateX1X2","rateX3","rateX1X3","rateX2X3","rateX1X2X3")

# Store the results
csvfile <- paste("logs/", filename, format(Sys.time(), "_%s"), ".csv", sep="")
write.csv(res, file = csvfile, row.names=FALSE, quote=F)

