# Author: Theophanis Tsandilas, 2023-2024
# Inria & Université Paris-Saclay

# Experiment evaluating the effect size estimates of PAR, ART, RNK, and INT.
# See Fig. 31, Fig. 32

# You can edit the experimental parameters in the code to test different conditions.


rm(list=ls())
library(lmerTest)
library(effectsize) # Effect size functions

# The ARTool
library(ARTool)

# Parallel computation
library(foreach)
library(doParallel)

source("data-generators.R") # Import the data-generation functions


eta_values <- function(model) {
	return(eta_squared(anova(model))[, 2])
}

cohensf_values <- function(model) {
	return(cohens_f(anova(model))[, 2])
}

r_squared <- function(pred, obs) {
	SS = sum((obs - pred)^2)
	SStotal = sum((obs - mean(pred))^2)

	return(1 - SS/SStotal)
}


# Repeated measures with two factors
test.within.2f <- function(n, design, effects, distr){ 
	df <- sim.within.2f(n, design, effects) %>% toDistribution(distr)

	model = suppressMessages(lmer(dv ~ x1*x2 + (1|s), data=df)) # This is the reference groundtruth model

	model0 <- suppressMessages(lmer(y ~ x1*x2 + (1|s), data=df)) # Parametric
	model1 <- suppressMessages(lmer(rank(y) ~ x1*x2 + (1|s), data=df)) # RNK
	model2 <- suppressMessages(art(y ~ x1*x2 + (1|s), data=df)) # ARTool
	model3 <- suppressMessages(lmer(inverseNormalTransform(y) ~ x1*x2 + (1|s), data=df)) # INT

	c(eta_values(model), eta_values(model0), eta_values(model1), eta_values(model2), eta_values(model3), 
	  cohensf_values(model), cohensf_values(model0), cohensf_values(model1), cohensf_values(model2), cohensf_values(model3))
}

# Mixed with one between-participants factor and one repeated-measures factor
test.mixed.2f <- function(n, design, effects, distr){
	df <- sim.mixed.2f(design[1]*n, design, effects) %>% toDistribution(distr) # Ensure n participants per cell

	model = suppressMessages(lmer(dv ~ x1*x2 + (1|s), data=df)) # This is the reference groundtruth model
	
	model0 <- suppressMessages(lmer(y ~ x1*x2 + (1|s), data=df)) # Parametric
	model1 <- suppressMessages(lmer(rank(y) ~ x1*x2 + (1|s), data=df)) # RNK
	model2 <- suppressMessages(art(y ~ x1*x2 + (1|s), data=df)) # ARTool
	model3 <- suppressMessages(lmer(inverseNormalTransform(y) ~ x1*x2 + (1|s), data=df)) # INT

	c(eta_values(model), eta_values(model0), eta_values(model1), eta_values(model2), eta_values(model3), 
	  cohensf_values(model), cohensf_values(model0), cohensf_values(model1), cohensf_values(model2), cohensf_values(model3))
}


# Between subjects with two factors
test.between.2f <- function(n, design, effects, distr){ 
	df <- sim.between.2f(design[1]*design[2]*n, design, effects) %>% toDistribution(distr)  # Ensure n participants per cell

	model <- suppressMessages(aov(dv ~ x1*x2, data=df))  # This is the reference groundtruth model

	model0 <- suppressMessages(aov(y ~ x1*x2, data=df)) # Parametric
	model1 <- suppressMessages(aov(rank(y) ~ x1*x2, data=df)) # RNK
	model2 <- suppressMessages(art(y ~ x1*x2, data=df)) # ARTool
	model3 <- suppressMessages(aov(inverseNormalTransform(y) ~ x1*x2, data=df)) # INT

	c(eta_values(model), eta_values(model0), eta_values(model1), eta_values(model2), eta_values(model3), 
	  cohensf_values(model), cohensf_values(model0), cohensf_values(model1), cohensf_values(model2), cohensf_values(model3))
}

# Choose the data generation function for each design
testFun <- function(id) {switch(id,test.between.2f, test.mixed.2f, test.within.2f)} 

# Performs repetitive tests for the given configuration
test <- function(repetitions, n, designId, design, distr, maxeffects) {
	results <- foreach(rid = 1:repetitions, .combine=rbind) %dopar% {
		effects = c(runif(1, min=-maxeffects[1], max=maxeffects[1]), runif(1, min=-maxeffects[2], max=maxeffects[2]), runif(1, min=-maxeffects[3], max=maxeffects[3]))

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
	
	designStr <- paste(design, collapse="x")
	len = 3

	# Split the results into separate rows 
	return(tribble(~n, ~design, ~distr, ~method, ~effects, ~eta, ~cohensf,
			n, designStr, distr, "PAR", maxeffects, sapply(1:len, function(x){ r_squared(results[,x], results[,x+len])}),sapply(1:len, function(x){ r_squared(results[,x+5*len], results[,x+6*len])}),		
			n, designStr, distr, "RNK", maxeffects, sapply(1:len, function(x){ r_squared(results[,x], results[,x+2*len])}), sapply(1:len, function(x){ r_squared(results[,x+5*len], results[,x+7*len])}),
			n, designStr, distr, "ART", maxeffects, sapply(1:len, function(x){ r_squared(results[,x], results[,3*len + x])}), sapply(1:len, function(x){ r_squared(results[,x+5*len], results[,x+8*len])}),
			n, designStr, distr, "INT", maxeffects, sapply(1:len, function(x){ r_squared(results[,x], results[,4*len + x])}), sapply(1:len, function(x){ r_squared(results[,x+5*len], results[,x+9*len])})
		)
	)
}

# Evaluate the following for main effects and interactions:
# A. Methods: (1) ANOVA/LMER with no transformation, (2) Rank, (3) ART, (4) INT. I test all three on the same datasets. 

# B.  2 x 3 between, 2 x 4 mixed design, and 4 x 3 repeated measures
designs <- list(c(2,3), c(2,4), c(4,3))

# C. Distributions
distributions <- c("norm", "lnorm", "exp", "poisson", "binom", "likert5B")

# D. I specify the range of effects for the two main effects and the interaction effect
maxeffects <- matrix(c(0, 0, 0,
					0.5, 0.5, 0.5,
					1, 1, 1,
					2, 2, 2,
				    4, 4, 4,
				    8, 8, 8), 
				   ncol = 3, byrow = TRUE)

# E. Cell sizes (n in the paper) -- for within-subject designs, it's also the number of subjects
#Ns <- c(10, 20, 30) 
Ns <- c(20) 

# correlations over 1000 data points
R <- 1000

filename = "6_test-Effect-Size"

# Set the seed for reproducibility
set.seed(2233)

#Parallel: https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html
CoresNum <- 4
registerDoParallel(CoresNum)  # use multicore, set to the number of our cores

time <- system.time({ 
	results <- foreach(desId = 1:length(designs), .combine=rbind) %do% {
		foreach(distr = distributions, .combine=rbind) %do% {
			foreach(n = Ns, .combine=rbind) %do% {
				foreach(effId = 1:nrow(maxeffects), .combine=rbind) %do% {
					# The test function is parallelized (using multiple cores)
					test(repetitions = R, n = n, designId = desId, design = designs[[desId]], distr = distr, maxeffects = maxeffects[effId,])
				}
			}
		}
	}
})

# split the effects and rates columns (currently vectors) into individual columns
res <- results %>% unnest_wider(effects, names_sep = "_") %>% unnest_wider(eta, names_sep = "_") %>% unnest_wider(cohensf, names_sep = "_")
colnames(res)[5:7]=c("effectX1","effectX2","effectX1X2")
colnames(res)[8:10]=c("etaX1","etaX2","etaX1X2")
colnames(res)[11:13]=c("cohensfX1","cohensfX2","cohensfX1X2")

# Store the results
csvfile <- paste("logs/", filename, format(Sys.time(), "_%s"), ".csv", sep="")
write.csv(res, file = csvfile, row.names=FALSE, quote=F)

