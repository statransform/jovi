# Author: Theophanis Tsandilas, 2023-2024
# Inria & Université Paris-Saclay

# Experiment evaluating the Type I error rates of PAR, ART, RNK, and INT when a random sample of 10% of the observations are missing 
# See Appendix: Fig.7, Fig.8, Fig.9

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

getLmerPValues <- function(model, vars, lmer = TRUE) {
	return(suppressMessages(anova(model)[vars, ifelse(lmer, 6, 5)])) # For aov models the table is different
}

getARTPValues <- function(model, vars, lmer = TRUE) {
	return(suppressMessages(anova(model)[vars, ifelse(lmer, 5, 7)])) # For aov models the ART's table is different
}


test.between.2f <- function(n, design, effects, distr, missing){
	df <- sim.between.2f(design[1]*design[2]*n, design, effects) %>% toDistribution(distr) %>% removeCells(missing)
	
	model0 <- suppressMessages(aov(y ~ x1*x2, data=df)) # Parametric
	model1 <- suppressMessages(aov(rank(y) ~ x1*x2, data=df)) # RNK
	model2 <- suppressMessages(art(y ~ x1*x2, data=df)) # ARTool
	model3 <- suppressMessages(aov(inverseNormalTransform(y) ~ x1*x2, data=df)) # INT

	vars <- c("x1", "x2", "x1:x2")	
	c(getLmerPValues(model0, vars, FALSE), getLmerPValues(model1, vars, FALSE), getARTPValues(model2, vars, FALSE), getLmerPValues(model3, vars, FALSE))
}


test.mixed.2f <- function(n, design, effects, distr, missing){
	df <- sim.mixed.2f(design[1]*n, design, effects) %>% toDistribution(distr)  %>% removeCells(missing)
	
	model0 <- suppressMessages(lmer(y ~ x1*x2 + (1|s), data=df)) # Parametric
	model1 <- suppressMessages(lmer(rank(y) ~ x1*x2 + (1|s), data=df)) # RNK
	model2 <- suppressMessages(art(y ~ x1*x2 + (1|s), data=df)) # ARTool
	model3 <- suppressMessages(lmer(inverseNormalTransform(y) ~ x1*x2 + (1|s), data=df)) # INT

	vars <- c("x1", "x2", "x1:x2")	
	c(getLmerPValues(model0, vars), getLmerPValues(model1, vars), getARTPValues(model2, vars), getLmerPValues(model3, vars))
}

# Repeated measures with two factors
test.within.2f <- function(n, design, effects, distr, missing){ 
	df <- sim.within.2f(n, design, effects) %>% toDistribution(distr) %>% removeCells(missing)

	model0 <- suppressMessages(lmer(y ~ x1*x2 + (1|s), data=df)) # Parametric
	model1 <- suppressMessages(lmer(rank(y) ~ x1*x2 + (1|s), data=df)) # RNK
	model2 <- suppressMessages(art(y ~ x1*x2 + (1|s), data=df)) # ARTool
	model3 <- suppressMessages(lmer(inverseNormalTransform(y) ~ x1*x2 + (1|s), data=df)) # INT
	vars <- c("x1", "x2", "x1:x2")
	c(getLmerPValues(model0, vars), getLmerPValues(model1, vars), getARTPValues(model2, vars), getLmerPValues(model3, vars))
}

# Choose the data generation function for each design
testFun <- function(id) {switch(id,test.between.2f, test.mixed.2f, test.within.2f)} 

# Performs repetitive tests for the given configuration
test <- function(repetitions, n, designId, design, effects, distr, sd_ratio, missing = 0.0) {
	results <- foreach(rid = 1:repetitions, .combine=rbind) %dopar% {
		tryCatch(
			{
				testFun(designId)(n, design, effects, distr, missing)
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

# Distributions to evaluate
distributions <- c("norm", "lnorm", "exp", "poisson", "binom", "likert5B")

# Various combinations of effects
effects <- matrix(c(0, 0, 0,
				    0.5, 0.5, 0,
				    1, 1, 0,
				   	2, 2, 0,
				    4, 4, 0,
				    8, 8, 0,
				    0.5, 0, 0,
				    1, 0, 0,
				    2, 0, 0,
				    4, 0, 0,
				    8, 0, 0), 
				   ncol = 3, byrow = TRUE)

# 2 x 3 between-subjects, 2 x 4 mixed-subjects, and 4x3 within-subjects designs
designs <- list(c(2,3), c(2,4), c(4,3))

n <- 20
R <- 5000

filename = "Appendix_test_missing"

missing_ratio <- 0.1 # Fix the percentage of missing data to 10% (randomly chosen)

# Set the seed for reproducibility
set.seed(1234)

#Parallel: https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html
CoresNum <- 4

registerDoParallel(CoresNum)  # use multicore, set to the number of our cores

time <- system.time({ 
	results <- foreach(desId = 1:length(designs), .combine=rbind) %do% {
		foreach(distr = distributions, .combine=rbind) %do% {
			foreach(effId = 1:nrow(effects), .combine=rbind) %do% {
				# The test function is parallelized (using multiple cores)
				test(repetitions = R, n = n, designId = desId, design = designs[[desId]], effects = effects[effId,], distr = distr, missing = missing_ratio)
			}
		}
	}
})

# split the effects and rates columns (currently vectors) into individual columns
res <- results %>% unnest_wider(effects, names_sep = "_") %>% unnest_wider(rates, names_sep = "_")
colnames(res)[6:8]=c("effectX1","effectX2","effectX1X2")
colnames(res)[9:11]=c("rateX1","rateX2","rateX1X2")

# Store the results
csvfile <- paste("logs/", filename, format(Sys.time(), "_%s"), ".csv", sep="")
write.csv(res, file = csvfile, row.names=FALSE, quote=F)

